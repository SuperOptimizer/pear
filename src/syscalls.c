#include "syscalls.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/utsname.h>
#include <sys/time.h>
#include <errno.h>
#include <time.h>

#ifdef __APPLE__
#include <mach-o/dyld.h>
#include <sys/syslimits.h>
#else
#include <linux/limits.h>
#endif

// Helper to read a string from guest memory
static int read_guest_string(rv_t* rv, u64 addr, char* buf, size_t max_len) {
    size_t i;
    for (i = 0; i < max_len - 1; i++) {
        u8 c;
        rv_res_e res = rv_vread8(rv, addr + i, &c);
        if (res != RV_RES_OK) return -1;
        buf[i] = c;
        if (c == 0) break;
    }
    buf[i] = 0;
    return 0;
}

// Helper to write data to guest memory
static int write_guest_mem(rv_t* rv, u64 addr, const void* data, size_t len) {
    const u8* src = (const u8*)data;
    for (size_t i = 0; i < len; i++) {
        rv_res_e res = rv_vwrite8(rv, addr + i, src[i]);
        if (res != RV_RES_OK) return -1;
    }
    return 0;
}

void syscall_init(syscall_ctx_t* ctx, u64 brk_start) {
    memset(ctx, 0, sizeof(*ctx));
    ctx->usermode = true;
    ctx->brk_addr = brk_start;
    ctx->mmap_base = 0x7f0000000000ULL;  // Start mmap allocations at high address
    ctx->next_fd = 3;  // 0, 1, 2 are stdin/stdout/stderr
}

// Convert Linux open flags to host flags
static int convert_open_flags(int linux_flags) {
    int host_flags = 0;
    
    // Basic access modes
    switch (linux_flags & 3) {
        case 0: host_flags |= O_RDONLY; break;
        case 1: host_flags |= O_WRONLY; break;
        case 2: host_flags |= O_RDWR; break;
    }
    
    // Common flags (these are usually the same across platforms)
    if (linux_flags & 0x40) host_flags |= O_CREAT;
    if (linux_flags & 0x80) host_flags |= O_EXCL;
    if (linux_flags & 0x200) host_flags |= O_TRUNC;
    if (linux_flags & 0x400) host_flags |= O_APPEND;
    if (linux_flags & 0x4000) host_flags |= O_NONBLOCK;
    
    return host_flags;
}

s64 handle_linux_syscall(rv_t* rv, syscall_ctx_t* ctx) {
    u64 syscall_num = rv->x[17];  // a7
    u64 arg0 = rv->x[10];  // a0
    u64 arg1 = rv->x[11];  // a1
    u64 arg2 = rv->x[12];  // a2
    u64 arg3 = rv->x[13];  // a3
    u64 arg4 = rv->x[14];  // a4
    u64 arg5 = rv->x[15];  // a5
    
    
    s64 ret = -ENOSYS;
    
    switch (syscall_num) {
    case SYS_exit:
        exit(arg0);
        break;
        
    case SYS_exit_group:
        exit(arg0);
        break;
        
    case SYS_write: {
        if (arg0 > 2 && arg0 < ctx->next_fd) {
            // For now, just handle stdin/stdout/stderr
            ret = -EBADF;
        } else {
            // Create a buffer and copy data from guest memory
            char* buf = malloc(arg2);
            if (!buf) {
                ret = -ENOMEM;
                break;
            }
            
            bool read_ok = true;
            for (u64 i = 0; i < arg2; i++) {
                u8 byte;
                if (rv_vread8(rv, arg1 + i, &byte) != RV_RES_OK) {
                    read_ok = false;
                    break;
                }
                buf[i] = byte;
            }
            
            if (read_ok) {
                ret = write(arg0, buf, arg2);
                if (ret < 0) ret = -errno;
            } else {
                ret = -EFAULT;
            }
            
            free(buf);
        }
        break;
    }
    
    case SYS_read: {
        if (arg0 > 2 && arg0 < ctx->next_fd) {
            ret = -EBADF;
        } else {
            char* buf = malloc(arg2);
            if (!buf) {
                ret = -ENOMEM;
                break;
            }
            
            ret = read(arg0, buf, arg2);
            if (ret < 0) {
                ret = -errno;
            } else if (ret > 0) {
                // Copy data back to guest memory
                if (write_guest_mem(rv, arg1, buf, ret) < 0) {
                    ret = -EFAULT;
                }
            }
            
            free(buf);
        }
        break;
    }
    
    case SYS_openat: {
        char path[PATH_MAX];
        if (read_guest_string(rv, arg1, path, sizeof(path)) < 0) {
            ret = -EFAULT;
            break;
        }
        
        int flags = convert_open_flags(arg2);
        int mode = arg3;
        
        if (arg0 != -100) {  // AT_FDCWD
            ret = -ENOSYS;  // TODO: implement directory fd support
        } else {
            int fd = open(path, flags, mode);
            if (fd < 0) {
                ret = -errno;
            } else {
                // For simplicity, return a fixed fd number
                close(fd);  // Close the real fd for now
                ret = ctx->next_fd++;
            }
        }
        break;
    }
    
    case SYS_close:
        if (arg0 < 3 || arg0 >= ctx->next_fd) {
            ret = -EBADF;
        } else {
            ret = 0;  // Pretend it worked
        }
        break;
        
    case SYS_brk: {
        if (arg0 == 0) {
            // Query current brk
            ret = ctx->brk_addr;
        } else if (arg0 >= ctx->brk_addr) {
            // Extend brk (simplified - doesn't actually allocate)
            ctx->brk_addr = arg0;
            ret = ctx->brk_addr;
        } else {
            // Can't shrink brk below initial value
            ret = ctx->brk_addr;
        }
        break;
    }
    
    case SYS_mmap: {
        u64 addr = arg0;
        u64 length = arg1;
        u64 prot = arg2;
        u64 flags = arg3;
        u64 fd = arg4;
        u64 offset = arg5;
        
        // Simplified mmap - just return sequential addresses
        if (fd != -1ULL || offset != 0) {
            ret = -ENOSYS;  // File mapping not supported yet
        } else if (length == 0) {
            ret = -EINVAL;
        } else {
            // Round up to page size
            length = (length + 4095) & ~4095ULL;
            ret = ctx->mmap_base;
            ctx->mmap_base += length;
        }
        break;
    }
    
    case SYS_munmap:
        // Pretend it worked
        ret = 0;
        break;
        
    case SYS_uname: {
        struct utsname un;
        if (uname(&un) < 0) {
            ret = -errno;
            break;
        }
        
        // Convert to Linux utsname structure
        struct {
            char sysname[65];
            char nodename[65];
            char release[65];
            char version[65];
            char machine[65];
            char domainname[65];
        } linux_un;
        
        memset(&linux_un, 0, sizeof(linux_un));
        strncpy(linux_un.sysname, "Linux", 64);  // Pretend to be Linux
        strncpy(linux_un.nodename, un.nodename, 64);
        strncpy(linux_un.release, "5.15.0", 64);
        strncpy(linux_un.version, "#1 SMP", 64);
        strncpy(linux_un.machine, "riscv64", 64);
        strncpy(linux_un.domainname, "(none)", 64);
        
        if (write_guest_mem(rv, arg0, &linux_un, sizeof(linux_un)) < 0) {
            ret = -EFAULT;
        } else {
            ret = 0;
        }
        break;
    }
    
    case SYS_getpid:
        ret = getpid();
        break;
        
    case SYS_getuid:
        ret = getuid();
        break;
        
    case SYS_geteuid:
        ret = geteuid();
        break;
        
    case SYS_getgid:
        ret = getgid();
        break;
        
    case SYS_getegid:
        ret = getegid();
        break;
        
    case SYS_gettimeofday: {
        struct timeval tv;
        if (gettimeofday(&tv, NULL) < 0) {
            ret = -errno;
        } else {
            // Linux timeval structure (64-bit tv_sec, 64-bit tv_usec)
            struct {
                s64 tv_sec;
                s64 tv_usec;
            } linux_tv = {
                .tv_sec = tv.tv_sec,
                .tv_usec = tv.tv_usec
            };
            
            if (write_guest_mem(rv, arg0, &linux_tv, sizeof(linux_tv)) < 0) {
                ret = -EFAULT;
            } else {
                ret = 0;
            }
        }
        break;
    }
    
    case SYS_clock_gettime: {
        struct timespec ts;
        clockid_t clk_id = arg0;
        
        // Convert Linux clock IDs to host
        if (clk_id == 0) clk_id = CLOCK_REALTIME;
        else if (clk_id == 1) clk_id = CLOCK_MONOTONIC;
        else {
            ret = -EINVAL;
            break;
        }
        
        if (clock_gettime(clk_id, &ts) < 0) {
            ret = -errno;
        } else {
            // Linux timespec structure (64-bit tv_sec, 64-bit tv_nsec)
            struct {
                s64 tv_sec;
                s64 tv_nsec;
            } linux_ts = {
                .tv_sec = ts.tv_sec,
                .tv_nsec = ts.tv_nsec
            };
            
            if (write_guest_mem(rv, arg1, &linux_ts, sizeof(linux_ts)) < 0) {
                ret = -EFAULT;
            } else {
                ret = 0;
            }
        }
        break;
    }
    
    case SYS_set_tid_address:
        // Just return a fake TID
        ret = 1;
        break;
        
    case SYS_rt_sigaction:
    case SYS_rt_sigprocmask:
        // Ignore signal operations for now
        ret = 0;
        break;
        
    case SYS_prlimit64:
        // Return dummy resource limits
        if (arg2 == 0 && arg3 != 0) {
            // Getting limits
            struct {
                u64 rlim_cur;
                u64 rlim_max;
            } rlim = {
                .rlim_cur = -1ULL,
                .rlim_max = -1ULL
            };
            if (write_guest_mem(rv, arg3, &rlim, sizeof(rlim)) < 0) {
                ret = -EFAULT;
            } else {
                ret = 0;
            }
        } else {
            ret = 0;
        }
        break;
        
    default:
        fprintf(stderr, "Unimplemented syscall: %llu\n", (unsigned long long)syscall_num);
        ret = -ENOSYS;
        break;
    }
    
    return ret;
}

// Simple ELF loader for usermode
err_e load_elf_usermode(rv_t* rv, syscall_ctx_t* ctx, const char* path) {
    FILE* f = fopen(path, "rb");
    if (!f) return ERROR;
    
    // Read ELF header
    struct {
        u8 e_ident[16];
        u16 e_type;
        u16 e_machine;
        u32 e_version;
        u64 e_entry;
        u64 e_phoff;
        u64 e_shoff;
        u32 e_flags;
        u16 e_ehsize;
        u16 e_phentsize;
        u16 e_phnum;
        u16 e_shentsize;
        u16 e_shnum;
        u16 e_shstrndx;
    } __attribute__((packed)) ehdr;
    
    if (fread(&ehdr, sizeof(ehdr), 1, f) != 1) {
        fclose(f);
        return ERROR;
    }
    
    // Verify ELF magic and architecture
    if (memcmp(ehdr.e_ident, "\x7f""ELF", 4) != 0 ||
        ehdr.e_ident[4] != 2 ||  // 64-bit
        ehdr.e_ident[5] != 1 ||  // Little endian
        ehdr.e_machine != 243) { // RISC-V
        fclose(f);
        return ERROR;
    }
    
    // Process program headers
    u64 max_addr = 0;
    for (int i = 0; i < ehdr.e_phnum; i++) {
        fseek(f, ehdr.e_phoff + i * ehdr.e_phentsize, SEEK_SET);
        
        struct {
            u32 p_type;
            u32 p_flags;
            u64 p_offset;
            u64 p_vaddr;
            u64 p_paddr;
            u64 p_filesz;
            u64 p_memsz;
            u64 p_align;
        } __attribute__((packed)) phdr;
        
        if (fread(&phdr, sizeof(phdr), 1, f) != 1) {
            fclose(f);
            return ERROR;
        }
        
        // PT_LOAD
        if (phdr.p_type == 1) {
            // For usermode, we load at low addresses directly
            // Check bounds
            if (phdr.p_vaddr + phdr.p_memsz > MACH_RAM_SIZE) {
                fprintf(stderr, "ELF segment too large: vaddr=%llx size=%llx\n", 
                        (unsigned long long)phdr.p_vaddr, 
                        (unsigned long long)phdr.p_memsz);
                fclose(f);
                return ERROR;
            }
            
            // Zero out the memory region first
            memset(rv->ram + phdr.p_vaddr, 0, phdr.p_memsz);
            
            // Load file contents from the file offset
            if (phdr.p_filesz > 0) {
                fseek(f, phdr.p_offset, SEEK_SET);
                if (fread(rv->ram + phdr.p_vaddr, phdr.p_filesz, 1, f) != 1) {
                    fclose(f);
                    return ERROR;
                }
            }
            
            u64 end_addr = phdr.p_vaddr + phdr.p_memsz;
            if (end_addr > max_addr) {
                max_addr = end_addr;
            }
        }
    }
    
    fclose(f);
    
    // Set up initial state
    rv->pc = ehdr.e_entry;
    
    // Set up initial stack at high address
    u64 stack_top = 0x7fffffffe000ULL;
    rv->x[2] = stack_top;  // sp
    
    // Initialize brk to end of loaded segments
    ctx->brk_addr = (max_addr + 4095) & ~4095ULL;
    
    return OK;
}
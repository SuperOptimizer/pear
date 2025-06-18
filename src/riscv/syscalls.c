#include "syscalls.h"
#include "rv32gc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/utsname.h>

// Simple memory management for user processes
static u32 user_brk = 0x40000000;  // Start of heap

void rv_handle_user_syscall(rv_t* rv) {
    u32 syscall_num = rv->a7;  // System call number in a7
    u32 arg0 = rv->a0;
    u32 arg1 = rv->a1;
    u32 arg2 = rv->a2;
    u32 arg3 = rv->a3;
    u32 arg4 = rv->a4;
    u32 arg5 = rv->a5;
    
    s32 result = 0;
    
    printf("SYSCALL: %u (0x%x, 0x%x, 0x%x, 0x%x, 0x%x, 0x%x)\n", 
           syscall_num, arg0, arg1, arg2, arg3, arg4, arg5);
    
    switch (syscall_num) {
        case SYS_exit:
        case SYS_exit_group:
            printf("Process exiting with code %d\n", arg0);
            exit(arg0);
            break;
            
        case SYS_write: {
            int fd = (int)arg0;
            u32 buf_addr = arg1;
            size_t count = arg2;
            
            if (fd == 1 || fd == 2) {  // stdout or stderr
                // Read data from emulated memory
                if (buf_addr + count < MEMORY_SIZE) {
                    char* buf = (char*)(rv->ram + buf_addr);
                    result = write(fd, buf, count);
                } else {
                    result = -EFAULT;
                }
            } else {
                result = -EBADF;
            }
            break;
        }
        
        case SYS_read: {
            int fd = (int)arg0;
            u32 buf_addr = arg1;
            size_t count = arg2;
            
            if (fd == 0) {  // stdin
                if (buf_addr + count < MEMORY_SIZE) {
                    char* buf = (char*)(rv->ram + buf_addr);
                    result = read(fd, buf, count);
                } else {
                    result = -EFAULT;
                }
            } else {
                result = -EBADF;
            }
            break;
        }
        
        case SYS_brk: {
            u32 new_brk = arg0;
            if (new_brk == 0) {
                // Return current break
                result = user_brk;
            } else {
                // Set new break
                if (new_brk < MEMORY_SIZE && new_brk >= 0x40000000) {
                    user_brk = new_brk;
                    result = new_brk;
                } else {
                    result = user_brk;  // Return old break on failure
                }
            }
            break;
        }
        
        case SYS_getpid:
            result = 1;  // Always return PID 1
            break;
            
        case SYS_getppid:
            result = 0;  // Parent PID 0
            break;
            
        case SYS_getuid:
        case SYS_geteuid:
            result = 1000;  // User ID 1000
            break;
            
        case SYS_getgid:
        case SYS_getegid:
            result = 1000;  // Group ID 1000
            break;
            
        case SYS_gettid:
            result = 1;  // Thread ID 1
            break;
            
        case SYS_gettimeofday: {
            u32 tv_addr = arg0;
            u32 tz_addr = arg1;
            
            if (tv_addr && tv_addr + 8 < MEMORY_SIZE) {
                struct timeval tv;
                gettimeofday(&tv, NULL);
                
                // Write timeval to emulated memory
                *(u32*)(rv->ram + tv_addr) = tv.tv_sec;
                *(u32*)(rv->ram + tv_addr + 4) = tv.tv_usec;
                result = 0;
            } else {
                result = -EFAULT;
            }
            break;
        }
        
        case SYS_uname: {
            u32 buf_addr = arg0;
            if (buf_addr && buf_addr + sizeof(struct utsname) < MEMORY_SIZE) {
                struct utsname* uts = (struct utsname*)(rv->ram + buf_addr);
                strcpy(uts->sysname, "Linux");
                strcpy(uts->nodename, "pear-rv32");
                strcpy(uts->release, "5.4.0");
                strcpy(uts->version, "#1 SMP");
                strcpy(uts->machine, "riscv32");
                // strcpy(uts->domainname, "(none)");  // Not available on all systems
                result = 0;
            } else {
                result = -EFAULT;
            }
            break;
        }
        
        case SYS_mmap: {
            u32 addr = arg0;
            u32 length = arg1;
            int prot = arg2;
            int flags = arg3;
            int fd = arg4;
            u32 offset = arg5;
            
            // Simple mmap implementation - just allocate at requested address
            if (addr == 0) {
                // Find free space
                addr = user_brk;
                user_brk += (length + 4095) & ~4095;  // Align to page boundary
            }
            
            if (addr + length < MEMORY_SIZE) {
                memset(rv->ram + addr, 0, length);
                result = addr;
            } else {
                result = -ENOMEM;
            }
            break;
        }
        
        case SYS_munmap:
            // Simple munmap - just return success
            result = 0;
            break;
            
        case SYS_close:
            // Simple close - return success for valid fds
            if ((int)arg0 >= 0 && (int)arg0 < 1024) {
                result = 0;
            } else {
                result = -EBADF;
            }
            break;
            
        default:
            printf("Unimplemented syscall: %u\n", syscall_num);
            result = -ENOSYS;
            break;
    }
    
    // Set return value in a0
    rv->a0 = result;
    
    // Advance PC past the syscall instruction
    rv->pc += 4;
}
#include "common.h"
#include "rv64gc.h"
#include "syscalls.h"
#include <string.h>

static void print_usage(const char* prog) {
    printf("Usage:\n");
    printf("  %s [options] <binary>\n", prog);
    printf("\nOptions:\n");
    printf("  --usermode    Run in usermode emulation (Linux ELF)\n");
    printf("  --cycles N    Maximum cycles to execute\n");
    printf("\nExamples:\n");
    printf("  %s --usermode ./hello\n", prog);
    printf("  %s ./tests/fibonacci.elf\n", prog);
}

int main(int argc, char* argv[]) {
    u64 max_cycles = 10000;
    const char *binary_path = NULL;
    const char *dtb_path = "linux/rv.dtb";
    bool usermode = false;
    
    // Parse arguments
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--usermode") == 0) {
            usermode = true;
        } else if (strcmp(argv[i], "--cycles") == 0 && i + 1 < argc) {
            max_cycles = (u64)atol(argv[++i]);
        } else if (strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-h") == 0) {
            print_usage(argv[0]);
            return 0;
        } else if (argv[i][0] != '-') {
            binary_path = argv[i];
        }
    }
    
    if (!binary_path) {
        print_usage(argv[0]);
        return 1;
    }
    
    printf("Pear RISC-V Emulator\n");
    printf("Mode: %s\n", usermode ? "Usermode (Linux ELF)" : "System");
    printf("Loading binary: %s\n", binary_path);
    
    rv_t *cpu = malloc(sizeof(rv_t));
    if (!cpu) {
        printf("Failed to allocate CPU structure\n");
        return 1;
    }
    
    rv_machine_init(cpu);
    if (!cpu->ram) {
        printf("Failed to allocate memory\n");
        free(cpu);
        return 1;
    }
    
    if (usermode) {
        // Usermode emulation
        syscall_ctx_t *ctx = malloc(sizeof(syscall_ctx_t));
        if (!ctx) {
            printf("Failed to allocate syscall context\n");
            rv_free(cpu);
            free(cpu);
            return 1;
        }
        
        syscall_init(ctx, 0x10000);  // Initial brk
        cpu->syscall_ctx = ctx;
        cpu->priv = RV_PRIV_USER;
        
        // Load ELF file
        if (load_elf_usermode(cpu, ctx, binary_path) != OK) {
            printf("Failed to load ELF file\n");
            free(ctx);
            rv_free(cpu);
            free(cpu);
            return 1;
        }
        
        printf("Entry point: 0x%016llx\n", (unsigned long long)cpu->pc);
        printf("Initial SP: 0x%016llx\n", (unsigned long long)cpu->x[2]);
    } else {
        // System mode - original behavior
        rv_machine_load_file(binary_path, cpu->ram, MACH_RAM_SIZE);
        
        // Only load DTB if not running a test program
        if (strstr(binary_path, "tests/") == NULL) {
            FILE *dtb_file = fopen(dtb_path, "rb");
            if (dtb_file) {
                fclose(dtb_file);
                rv_machine_load_file(dtb_path, cpu->ram + MACH_DTB_OFFSET, MACH_RAM_SIZE - MACH_DTB_OFFSET);
            } else {
                printf("Warning: DTB file %s not found\n", dtb_path);
            }
        }
        
        // Set up initial register state for Linux boot
        cpu->x[10] = 0; // hartid (hardware thread ID)
        cpu->x[11] = MACH_RAM_BASE + MACH_DTB_OFFSET; // DTB pointer
        
        printf("Starting emulation at PC: 0x%016llx\n", (unsigned long long)cpu->pc);
        printf("Hart ID: %llu, DTB: 0x%016llx\n", (unsigned long long)cpu->x[10], (unsigned long long)cpu->x[11]);
    }
    
    u64 result = rv_machine_run(cpu, max_cycles);
    
    printf("Emulation finished with result: 0x%016llx\n", (unsigned long long)result);
    
    if (cpu->syscall_ctx) {
        free(cpu->syscall_ctx);
    }
    
    rv_free(cpu);
    free(cpu);
    return 0;
}
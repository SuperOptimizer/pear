#include "common.h"
#include "riscv/rv32gc.h"
#include "riscv/syscalls.h"
#include "elf.h"


int is_elf_file(const char* filename) {
    FILE* f = fopen(filename, "rb");
    if (!f) return 0;
    
    u8 header[64];  // Read enough for full ELF header
    size_t read_size = fread(header, 1, sizeof(header), f);
    fclose(f);
    
    if (read_size < sizeof(header)) return 0;
    
    return elf_is_valid(header, read_size);
}

int load_binary(const char* filename, rv_t* rv, u32 load_addr) {
    FILE* f = fopen(filename, "rb");
    if (!f) return -1;

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    if (load_addr + size >= MEMORY_SIZE) {
        fclose(f);
        return -1;
    }

    fread(rv->ram + load_addr, 1, size, f);
    fclose(f);

    printf("Loaded %zu bytes at 0x%08x\n", size, load_addr);
    return 0;
}

// Initialize emulator for kernel boot
void setup_kernel_boot(rv_t* rv) {
    // Initialize all state
    memset(rv, 0, sizeof(rv_t));
    rv_csr_init(rv);

    // Set up boot state
    rv->pc = 0x80200000;              // Kernel entry point
    rv->a0 = 0;                       // Hart ID
    rv->a1 = 0x82000000;             // Device tree address
    rv->sp = 0x80000000;              // Initial stack pointer

    // Start in machine mode
    rv->csr.priv = PRIV_MACHINE;

    // Set up basic machine state
    rv->csr.mstatus = 0;
    rv->csr.mtvec = 0x80000000;       // Machine trap vector
    rv->csr.stvec = 0x80200000;       // Supervisor trap vector

    printf("Emulator initialized for kernel boot\n");
    printf("PC: 0x%08x, Hart ID: %u, DTB: 0x%08x\n", rv->pc, rv->a0, rv->a1);
}

// Exception handler for debugging
void handle_exception(rv_t* rv, u32 cause, u32 tval) {
    printf("EXCEPTION: ");
    switch (cause) {
        case CAUSE_MISALIGNED_FETCH:    printf("Instruction address misaligned"); break;
        case CAUSE_FETCH_ACCESS:        printf("Instruction access fault"); break;
        case CAUSE_ILLEGAL_INSTRUCTION: printf("Illegal instruction"); break;
        case CAUSE_BREAKPOINT:          printf("Breakpoint"); break;
        case CAUSE_MISALIGNED_LOAD:     printf("Load address misaligned"); break;
        case CAUSE_LOAD_ACCESS:         printf("Load access fault"); break;
        case CAUSE_MISALIGNED_STORE:    printf("Store address misaligned"); break;
        case CAUSE_STORE_ACCESS:        printf("Store access fault"); break;
        case CAUSE_USER_ECALL:          printf("User ecall"); break;
        case CAUSE_SUPERVISOR_ECALL:    printf("Supervisor ecall"); break;
        case CAUSE_MACHINE_ECALL:       printf("Machine ecall"); break;
        case CAUSE_FETCH_PAGE_FAULT:    printf("Instruction page fault"); break;
        case CAUSE_LOAD_PAGE_FAULT:     printf("Load page fault"); break;
        case CAUSE_STORE_PAGE_FAULT:    printf("Store page fault"); break;
        default: printf("Unknown cause %u", cause); break;
    }
    printf(" at PC=0x%08x, tval=0x%08x\n", rv->pc, tval);

    if (cause == CAUSE_SUPERVISOR_ECALL) {
        // Handle supervisor system calls
        rv_syscall_entry(rv, cause);
    } else if (cause == CAUSE_USER_ECALL) {
        // Handle user system calls
        rv_handle_user_syscall(rv);
    }
}

// Run emulator with exception handling
int run_emulator(rv_t* rv, u32 max_instructions) {
    u32 instruction_count = 0;
    
    printf("Starting execution: PC=0x%08x, priv=%u\n", rv->pc, rv->csr.priv);

    while (instruction_count < max_instructions) {
        // Check for exceptions before execution
        if (rv->csr.mcause != 0) {
            handle_exception(rv, rv->csr.mcause, rv->csr.mtval);
            rv->csr.mcause = 0; // Clear exception
        }

        // Fetch and disassemble instruction before execution
        u32 opcode = rv_fetch_instruction(rv, rv->pc);
        
        // Show disassembly for first 20 instructions
        if (instruction_count < 20) {
            rv_print_instruction(rv, opcode, rv->pc);
        }

        // Execute one step
        int result = rv_step(rv);
        if (result != 0) {
            printf("Execution stopped with result %d\n", result);
            break;
        }

        instruction_count++;

        // Debug output for first few instructions and then every 10000
        if (instruction_count <= 10 || instruction_count % 10000 == 0) {
            printf("PC: 0x%08x, instructions: %u, priv: %u\n",
                   rv->pc, instruction_count, rv->csr.priv);
        }

        // Stop if we hit invalid PC
        if (rv->pc == 0 || rv->pc >= MEMORY_SIZE) {
            printf("Invalid PC: 0x%08x, stopping\n", rv->pc);
            break;
        }
    }

    printf("Executed %u instructions\n", instruction_count);
    return 0;
}


int main(int argc, char* argv[]) {
    printf("RV32GC Full System Emulator\n");
    
    char* path = "";
    if (argc == 1) {
        path = "kernel.bin";
    } else {
        path = argv[1];
    }

    rv_t rv;
    rv_init(&rv);

    // Check if file is ELF or raw binary
    if (is_elf_file(path)) {
        printf("Detected ELF file: %s\n", path);
        
        // Load user ELF
        if (rv_load_user_elf(&rv, path) != 0) {
            printf("Failed to load ELF: %s\n", path);
            rv_free(&rv);
            return 1;
        }
        
        printf("Running user mode program...\n");
    } else {
        printf("Detected raw binary: %s\n", path);
        
        // Load kernel binary
        if (load_binary(path, &rv, 0x80200000) != 0) {
            printf("Failed to load kernel: %s\n", path);
            rv_free(&rv);
            return 1;
        }

        // Set up for kernel boot
        setup_kernel_boot(&rv);
        printf("Running kernel...\n");
    }

    // Run emulator
    run_emulator(&rv, 1000000);

    rv_free(&rv);
    return 0;
}

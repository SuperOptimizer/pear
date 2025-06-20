// Test program for usermode emulation

// Linux syscall numbers for RISC-V
#define SYS_write 64
#define SYS_exit 93

// Simple syscall wrapper
static long linux_syscall(long num, long arg1, long arg2, long arg3) {
    register long a0 asm("a0") = arg1;
    register long a1 asm("a1") = arg2; 
    register long a2 asm("a2") = arg3;
    register long a7 asm("a7") = num;
    
    asm volatile (
        "ecall"
        : "+r" (a0)
        : "r" (a7), "r" (a1), "r" (a2)
        : "memory"
    );
    
    return a0;
}

const char msg[] = "Hello from RISC-V usermode!\n";

void _start() {
    // Test write syscall
    linux_syscall(SYS_write, 1, (long)msg, sizeof(msg) - 1);
    
    // Test exit syscall
    linux_syscall(SYS_exit, 0, 0, 0);
    
    // Should never reach here
    while (1);
}
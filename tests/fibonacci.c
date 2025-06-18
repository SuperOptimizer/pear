/* Fibonacci test program for RISC-V emulator */

// System call numbers
#define SYS_exit 93
#define SYS_write 64

// System call wrapper
static inline long syscall(long num, long arg0, long arg1, long arg2) {
    register long a0 asm("a0") = arg0;
    register long a1 asm("a1") = arg1;
    register long a2 asm("a2") = arg2;
    register long a7 asm("a7") = num;
    
    asm volatile ("ecall"
                  : "=r"(a0)
                  : "r"(a0), "r"(a1), "r"(a2), "r"(a7)
                  : "memory");
    return a0;
}

// Simple print function
void print_string(const char* str) {
    const char* p = str;
    int len = 0;
    
    // Calculate length
    while (*p++) len++;
    
    syscall(SYS_write, 1, (long)str, len);
}

void print_number(int num) {
    char buffer[20];
    char* p = buffer + sizeof(buffer) - 1;
    *p = '\0';
    
    if (num == 0) {
        *(--p) = '0';
    } else {
        int is_negative = 0;
        if (num < 0) {
            is_negative = 1;
            num = -num;
        }
        
        while (num > 0) {
            *(--p) = '0' + (num % 10);
            num /= 10;
        }
        
        if (is_negative) {
            *(--p) = '-';
        }
    }
    
    print_string(p);
}

// Fibonacci calculation
int fibonacci(int n) {
    if (n <= 1) return n;
    
    int a = 0, b = 1, temp;
    for (int i = 2; i <= n; i++) {
        temp = a + b;
        a = b;
        b = temp;
    }
    return b;
}

// Entry point
void _start() {
    print_string("Starting fibonacci calculation...\n");
    
    int result = fibonacci(100);
    
    print_string("Fibonacci(100) = ");
    print_number(result);
    print_string("\n");
    
    print_string("Test completed successfully!\n");
    
    // Exit with the fibonacci result as return code (modulo 256)
    syscall(SYS_exit, result & 0xFF, 0, 0);
    
    // Should never reach here
    while (1);
}
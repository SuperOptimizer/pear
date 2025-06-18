#define SYS_exit 93

static inline long syscall(long num, long arg0, long arg1, long arg2) {
    register long result;
    asm volatile ("mv a7, %1\n\t"
                  "mv a0, %2\n\t"
                  "mv a1, %3\n\t"
                  "mv a2, %4\n\t"
                  "ecall\n\t"
                  "mv %0, a0"
                  : "=r"(result)
                  : "r"(num), "r"(arg0), "r"(arg1), "r"(arg2)
                  : "a0", "a1", "a2", "a7", "memory");
    return result;
}

void _start() {
    syscall(SYS_exit, 42, 0, 0);
    while (1);
}
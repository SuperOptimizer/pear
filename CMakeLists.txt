cmake_minimum_required(VERSION 3.20)

project(pear C)

add_compile_options(-Wall -Wextra -Wno-gcc-compat)

add_executable(pear 
    src/main.c 
    src/common.h 
    src/elf.c 
    src/elf.h
        src/riscv/syscalls.c
        src/riscv/syscalls.h
    src/riscv/rv32gc.c
    src/riscv/rv32gc.h
    src/riscv/mem.c
    src/riscv/mem.h
    src/riscv/csr.c
    src/riscv/csr.h
    src/riscv/interp.c
    src/riscv/interp.h
    src/riscv/disasm.c
    src/riscv/disasm.h
)


# Add tests subdirectory
add_subdirectory(tests)
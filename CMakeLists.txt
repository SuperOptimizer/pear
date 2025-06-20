cmake_minimum_required(VERSION 3.20)

project(pear C)

add_compile_options(-Wall
        -Wextra
        -Wno-gcc-compat
-Wno-unused-variable
-Wno-unused-parameter
-g3
-fsanitize=address,undefined
-Og)

add_link_options(-fsanitize=address,undefined -fuse-ld=lld)

add_executable(pear 
    src/main.c
    src/common.h
    src/rv64gc.c
    src/rv64gc.h
    src/rv64_opcodes.h
    src/rv64_decode.c
    src/rv64_disasm.c
    src/rv64_interp.c
    src/rv64_hw.c
    src/rv64_hw.h
    src/syscalls.c
    src/syscalls.h
)


# Add tests subdirectory
add_subdirectory(tests)
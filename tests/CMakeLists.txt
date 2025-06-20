set(RISCV_CC riscv64-elf-gcc)

set(RISCV_USER_CFLAGS
    -march=rv64gc
    -mabi=lp64
    -static
    -nostdlib
    -fno-builtin
    -O2
    -g
)

function(add_riscv_user_test name source)
    add_custom_command(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${name}
        COMMAND ${RISCV_CC} ${RISCV_USER_CFLAGS}
                -T${CMAKE_CURRENT_SOURCE_DIR}/linker.ld
                ${CMAKE_CURRENT_SOURCE_DIR}/${source}
                -o ${CMAKE_CURRENT_BINARY_DIR}/${name}
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${source} ${CMAKE_CURRENT_SOURCE_DIR}/linker.ld
        COMMENT "Building usermode RISC-V test: ${name}"
        VERBATIM
    )
    
    add_custom_target(${name} ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${name})
endfunction()

# Add usermode test programs
add_riscv_user_test(hello_usermode hello_usermode.c)
add_riscv_user_test(simple_exit simple_exit.c)
add_riscv_user_test(fibonacci fibonacci.c)

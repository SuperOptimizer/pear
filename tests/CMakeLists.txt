# RISC-V Test Programs CMake Configuration

# Set RISC-V compiler
set(RISCV_CC riscv64-elf-gcc)

# RISC-V compilation flags
set(RISCV_CFLAGS
    -march=rv32gc
    -mabi=ilp32
    -nostdlib
    -nostartfiles
    -static
    -fno-builtin
    -O0
    -g
    -fPIC
    -fPIE
)

# RISC-V linker flags  
set(RISCV_LDFLAGS
    -T${CMAKE_CURRENT_SOURCE_DIR}/linker.ld
    -nostdlib
    -static
    -static-pie
)

# Function to create RISC-V test programs
function(add_riscv_test name source)
    add_custom_command(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${name}.elf
        COMMAND ${RISCV_CC} ${RISCV_CFLAGS} ${RISCV_LDFLAGS} 
                ${CMAKE_CURRENT_SOURCE_DIR}/${source}
                -o ${CMAKE_CURRENT_BINARY_DIR}/${name}.elf
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${source} ${CMAKE_CURRENT_SOURCE_DIR}/linker.ld
        COMMENT "Building RISC-V test: ${name}"
        VERBATIM
    )
    
    add_custom_target(${name} ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${name}.elf)
    
    # Add test to run with emulator
    add_custom_target(run_${name}
        COMMAND ${CMAKE_BINARY_DIR}/pear ${CMAKE_CURRENT_BINARY_DIR}/${name}.elf
        DEPENDS pear ${name}
        COMMENT "Running RISC-V test: ${name}"
        VERBATIM
    )
endfunction()

# Add test programs
add_riscv_test(fibonacci fibonacci.c)

# Add a target to build all tests
add_custom_target(riscv_tests DEPENDS fibonacci)

# Add a target to run all tests
add_custom_target(run_riscv_tests DEPENDS run_fibonacci)
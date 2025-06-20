# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Pear is a RISC-V RV64GC emulator written in C. It implements a complete 64-bit RISC-V processor capable of executing RISC-V assembly code and ELF binaries.

## Build Commands

```bash
# Build the project
cmake -B cmake-build-debug
cmake --build cmake-build-debug

# Build and run test programs
cmake --build cmake-build-debug --target riscv_tests

# Run the emulator with test programs
./cmake-build-debug/pear ./cmake-build-debug/tests/fibonacci.elf
./cmake-build-debug/pear ./cmake-build-debug/tests/simple_exit.elf

# Run individual test targets
cmake --build cmake-build-debug --target run_fibonacci
cmake --build cmake-build-debug --target run_simple_exit
```

## Architecture

### Core Components

- **src/main.c**: Entry point and program setup
- **src/rv64gc.h/rv64gc.c**: Core RISC-V processor implementation including:
  - Instruction decoding and execution
  - Register file management
  - Memory operations
  - CSR (Control and Status Register) handling
- **src/common.h**: Type definitions and common utilities
- **tests/**: RISC-V test programs compiled with cross-compiler

### Key Data Structures

- `rv_t`: Main processor state containing:
  - 32 64-bit integer registers (x0-x31) with named aliases
  - 64-bit program counter (pc)
  - RAM pointer for memory space
  - Atomic reservation tracking
  - CSR state
- `rv_csr_t`: Complete CSR register implementation for machine mode

### Memory Layout

- 256MB physical memory (MEMORY_SIZE)
- Direct memory access through rv_read/rv_write functions
- Memory allocated per rv_t instance (rv->ram)

## Test Infrastructure

The project includes a custom CMake-based test system that:
- Cross-compiles C programs to RISC-V ELF binaries using `riscv64-elf-gcc`
- Uses custom linker script (`tests/linker.ld`)
- Provides targets to run tests with the emulator
- Test programs use system calls for basic I/O and program termination

## Development Notes

- The emulator uses instruction decoding with opcode pattern matching
- Supports full RV64GC (RV64IMAFDC) instruction set:
  - RV64I: Base 64-bit integer instruction set
  - M: Integer multiplication and division
  - A: Atomic instructions
  - F: Single-precision floating-point
  - D: Double-precision floating-point
  - C: Compressed 16-bit instructions
- Memory operations are direct pointer access to allocated RAM
- CSR operations are centralized through rv_csr_read/write functions
- Register x0 (zero) is handled specially and always returns 0
- The processor state is completely contained in the rv_t structure for easy management
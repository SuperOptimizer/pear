# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Pear is a RISC-V RV32GC full system emulator written in C. It implements a complete RISC-V processor with:
- RV32I base integer instruction set
- M extension (multiply/divide)
- A extension (atomic operations) 
- F extension (single-precision floating-point)
- D extension (double-precision floating-point)
- C extension (compressed instructions)
- User, Supervisor and Machine privilege levels
- Virtual memory with Sv32 page translation
- CSR (Control and Status Registers) implementation
- ELF file loading and execution
- Linux system call interface

The emulator can run both Linux kernels (raw binary format) and user mode Linux programs (ELF format).

## Build Commands

```bash
# Build the project
mkdir -p cmake-build-debug
cd cmake-build-debug
cmake ..
make

# Or use CMake directly from project root
cmake -B cmake-build-debug
cmake --build cmake-build-debug

# Run with kernel binary (raw format)
./cmake-build-debug/pear kernel.bin

# Run with user ELF program
./cmake-build-debug/pear program.elf

# Run without arguments (defaults to kernel.bin)
./cmake-build-debug/pear
```

## Architecture

### Core Components

- **main.c**: Entry point, file format detection, emulator setup, and debugging utilities
- **rv32gc.h/rv32gc.c**: Core RISC-V processor implementation including:
  - Instruction decoding and execution
  - CSR handling
  - Memory management (physical and virtual)  
  - Exception and interrupt handling
  - Privilege level management
  - User mode process setup
- **elf.h/elf.c**: ELF file parsing and loading for user programs
- **syscalls.h/syscalls.c**: Linux system call implementation for user programs
- **common.h**: Type definitions and common macros

### Key Data Structures

- `rv_t`: Main processor state containing registers, CSRs, PC, and memory pointer
- `csr_state_t`: Complete CSR register file for machine and supervisor modes
- `fpr_t`: Floating-point register union supporting both f32 and f64

### Memory Layout

- 256MB physical memory (MEMORY_SIZE)
- Kernel loads at 0x80200000
- Device tree at 0x82000000
- Initial stack at 0x80000000

### Privilege and Virtual Memory

- Supports User (0), Supervisor (1), and Machine (3) privilege levels
- Sv32 virtual memory with 4KB pages and 4MB megapages
- MMU translation with page table walking
- Exception delegation between privilege levels

## Development Notes

- The emulator automatically detects ELF vs raw binary files and sets up appropriate execution environment
- Memory is allocated per rv_t instance (rv->ram) rather than globally
- User programs run in User privilege mode with system call support
- Kernel binaries run in Machine mode with supervisor capabilities
- Instruction execution happens in rv_step() with exception handling
- CSR operations are centralized through rv_csr_read/write functions
- Memory access goes through translation layer that handles both physical and virtual addressing
- Exception handling supports proper trap delegation and privilege transitions
- System calls are handled through USER_ECALL exceptions with Linux ABI compatibility

## Supported System Calls

Basic Linux system calls are implemented including:
- Process control: exit, exit_group, getpid, getppid
- I/O operations: read, write (stdin/stdout/stderr)
- Memory management: brk, mmap, munmap
- Time functions: gettimeofday
- System information: uname, getuid, getgid
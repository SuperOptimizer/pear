#pragma once

#include "common.h"

// ELF Header
typedef struct {
    u8  e_ident[16];
    u16 e_type;
    u16 e_machine;
    u32 e_version;
    u32 e_entry;
    u32 e_phoff;
    u32 e_shoff;
    u32 e_flags;
    u16 e_ehsize;
    u16 e_phentsize;
    u16 e_phnum;
    u16 e_shentsize;
    u16 e_shnum;
    u16 e_shstrndx;
} elf32_ehdr_t;

// Program Header
typedef struct {
    u32 p_type;
    u32 p_offset;
    u32 p_vaddr;
    u32 p_paddr;
    u32 p_filesz;
    u32 p_memsz;
    u32 p_flags;
    u32 p_align;
} elf32_phdr_t;

// ELF constants
#define EI_MAG0       0
#define EI_MAG1       1
#define EI_MAG2       2
#define EI_MAG3       3
#define EI_CLASS      4
#define EI_DATA       5
#define EI_VERSION    6
#define EI_OSABI      7
#define EI_ABIVERSION 8

#define ELFMAG0       0x7f
#define ELFMAG1       'E'
#define ELFMAG2       'L'
#define ELFMAG3       'F'

#define ELFCLASS32    1
#define ELFDATA2LSB   1

#define ET_EXEC       2
#define ET_DYN        3

#define EM_RISCV      243

#define PT_NULL       0
#define PT_LOAD       1
#define PT_DYNAMIC    2
#define PT_INTERP     3
#define PT_NOTE       4

#define PF_X          1
#define PF_W          2
#define PF_R          4

// Function declarations
int elf_is_valid(const u8* data, size_t size);
int elf_load(u8* dest_ram, size_t dest_size, const char* filename, u32* entry_point, u32* stack_top);
int elf_load_from_data(u8* dest_ram, size_t dest_size, const u8* data, size_t size, u32* entry_point, u32* stack_top);
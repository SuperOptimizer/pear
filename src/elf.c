#include "elf.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int elf_is_valid(const u8* data, size_t size) {
    if (size < sizeof(elf32_ehdr_t)) {
        printf("ELF validation failed: size too small (%zu < %zu)\n", size, sizeof(elf32_ehdr_t));
        return 0;
    }
    
    const elf32_ehdr_t* header = (const elf32_ehdr_t*)data;
    
    // Check magic number
    if (header->e_ident[EI_MAG0] != ELFMAG0 ||
        header->e_ident[EI_MAG1] != ELFMAG1 ||
        header->e_ident[EI_MAG2] != ELFMAG2 ||
        header->e_ident[EI_MAG3] != ELFMAG3) {
        printf("ELF validation failed: bad magic %02x %02x %02x %02x\n", 
               header->e_ident[EI_MAG0], header->e_ident[EI_MAG1], 
               header->e_ident[EI_MAG2], header->e_ident[EI_MAG3]);
        return 0;
    }
    
    // Check class and data encoding
    if (header->e_ident[EI_CLASS] != ELFCLASS32 ||
        header->e_ident[EI_DATA] != ELFDATA2LSB) {
        printf("ELF validation failed: class=%d data=%d\n", 
               header->e_ident[EI_CLASS], header->e_ident[EI_DATA]);
        return 0;
    }
    
    // Check machine type (handle endianness)
    u16 machine = header->e_machine;
    if (header->e_ident[EI_DATA] == ELFDATA2LSB) {
        // Convert from little-endian if needed
        machine = (data[18] | (data[19] << 8));
    }
    
    printf("ELF machine type: %d (expected %d)\n", machine, EM_RISCV);
    
    if (machine != EM_RISCV) {
        return 0;
    }
    
    // Check type (executable or shared object)
    if (header->e_type != ET_EXEC && header->e_type != ET_DYN) {
        return 0;
    }
    
    return 1;
}

int elf_load_from_data(u8* dest_ram, size_t dest_size, const u8* data, size_t size, u32* entry_point, u32* stack_top) {
    if (!elf_is_valid(data, size)) {
        printf("Invalid ELF file\n");
        return -1;
    }
    
    const elf32_ehdr_t* header = (const elf32_ehdr_t*)data;
    *entry_point = header->e_entry;
    
    printf("Loading ELF: entry=0x%08x, %d program headers\n", 
           header->e_entry, header->e_phnum);
    
    // Process program headers
    for (int i = 0; i < header->e_phnum; i++) {
        const elf32_phdr_t* phdr = (const elf32_phdr_t*)(data + header->e_phoff + i * header->e_phentsize);
        
        if (phdr->p_type == PT_LOAD) {
            printf("Loading segment: vaddr=0x%08x, memsz=%u, filesz=%u, flags=0x%x\n",
                   phdr->p_vaddr, phdr->p_memsz, phdr->p_filesz, phdr->p_flags);
            
            // Check if segment fits in memory
            if (phdr->p_vaddr + phdr->p_memsz >= dest_size) {
                printf("Segment too large for memory\n");
                return -1;
            }
            
            // Zero the memory region
            memset(dest_ram + phdr->p_vaddr, 0, phdr->p_memsz);
            
            // Copy file data if present
            if (phdr->p_filesz > 0) {
                if (phdr->p_offset + phdr->p_filesz > size) {
                    printf("Segment data extends beyond file\n");
                    return -1;
                }
                memcpy(dest_ram + phdr->p_vaddr, data + phdr->p_offset, phdr->p_filesz);
            }
        }
    }
    
    // Set up user stack at high memory (below kernel space)
    *stack_top = 0x7fff0000;
    
    return 0;
}

int elf_load(u8* dest_ram, size_t dest_size, const char* filename, u32* entry_point, u32* stack_top) {
    FILE* f = fopen(filename, "rb");
    if (!f) {
        printf("Failed to open ELF file: %s\n", filename);
        return -1;
    }
    
    // Get file size
    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);
    
    // Read entire file
    u8* data = malloc(size);
    if (!data) {
        printf("Failed to allocate memory for ELF file\n");
        fclose(f);
        return -1;
    }
    
    if (fread(data, 1, size, f) != size) {
        printf("Failed to read ELF file\n");
        free(data);
        fclose(f);
        return -1;
    }
    
    fclose(f);
    
    int result = elf_load_from_data(dest_ram, dest_size, data, size, entry_point, stack_top);
    free(data);
    
    return result;
}
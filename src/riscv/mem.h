#pragma once

#include "../common.h"
#include "rv32gc.h"
// Physical memory access functions
u8 rv_phys_read8(rv_t *rv, u32 paddr);
u16 rv_phys_read16(rv_t *rv, u32 paddr);
u32 rv_phys_read32(rv_t *rv, u32 paddr);
void rv_phys_write8(rv_t *rv, u32 paddr, u8 val);
void rv_phys_write16(rv_t *rv, u32 paddr, u16 val);
void rv_phys_write32(rv_t *rv, u32 paddr, u32 val);

// Virtual memory access functions
u8 rv_mem_read8(rv_t *rv, u32 addr);
u16 rv_mem_read16(rv_t *rv, u32 addr);
u32 rv_mem_read32(rv_t *rv, u32 addr);
void rv_mem_write8(rv_t *rv, u32 addr, u8 val);
void rv_mem_write16(rv_t *rv, u32 addr, u16 val);
void rv_mem_write32(rv_t *rv, u32 addr, u32 val);

// Instruction fetch
u32 rv_fetch_instruction(rv_t *rv, u32 addr);

// Virtual memory translation
int rv_translate_address(rv_t *rv, u32 vaddr, u32* paddr, int is_write, int is_exec);

// Floating point memory operations
f64 rv_mem_read_f64(rv_t *rv, u32 addr);
void rv_mem_write_f64(rv_t *rv, u32 addr, f64 val);
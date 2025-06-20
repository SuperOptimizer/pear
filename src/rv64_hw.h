#pragma once

#include "common.h"
#include "rv64gc.h"

// PLIC functions
void rv_plic_init(rv_plic* plic);
u8 rv_plic_read8(rv_t* rv, u64 addr);
u16 rv_plic_read16(rv_t* rv, u64 addr);
u32 rv_plic_read32(rv_t* rv, u64 addr);
void rv_plic_write8(rv_t* rv, u64 addr, u8 data);
void rv_plic_write16(rv_t* rv, u64 addr, u16 data);
void rv_plic_write32(rv_t* rv, u64 addr, u32 data);
rv_res_e rv_plic_irq(rv_t* rv, u32 source);
u32 rv_plic_mei(rv_t* rv, u32 context);

// CLINT functions
void rv_clint_init(rv_clint* clint);
u8 rv_clint_read8(rv_t* rv, u64 addr);
u16 rv_clint_read16(rv_t* rv, u64 addr);
u32 rv_clint_read32(rv_t* rv, u64 addr);
void rv_clint_write8(rv_t* rv, u64 addr, u8 data);
void rv_clint_write16(rv_t* rv, u64 addr, u16 data);
void rv_clint_write32(rv_t* rv, u64 addr, u32 data);
u32 rv_clint_msi(rv_t* rv, u32 context);
u32 rv_clint_mti(rv_t* rv, u32 context);

// UART functions
void rv_uart_init(rv_uart* uart);
u8 rv_uart_read8(rv_t* rv, u64 addr);
u16 rv_uart_read16(rv_t* rv, u64 addr);
u32 rv_uart_read32(rv_t* rv, u64 addr);
void rv_uart_write8(rv_t* rv, u64 addr, u8 data);
void rv_uart_write16(rv_t* rv, u64 addr, u16 data);
void rv_uart_write32(rv_t* rv, u64 addr, u32 data);

// Machine-level memory access functions
u8 rv_machine_read8(rv_t* rv, u64 addr);
u16 rv_machine_read16(rv_t* rv, u64 addr);
u32 rv_machine_read32(rv_t* rv, u64 addr);
u64 rv_machine_read64(rv_t* rv, u64 addr);
void rv_machine_write8(rv_t* rv, u64 addr, u8 data);
void rv_machine_write16(rv_t* rv, u64 addr, u16 data);
void rv_machine_write32(rv_t* rv, u64 addr, u32 data);
void rv_machine_write64(rv_t* rv, u64 addr, u64 data);
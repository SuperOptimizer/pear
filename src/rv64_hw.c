#include <string.h>
#include <stdio.h>
#include "rv64_hw.h"
#include "rv64gc.h"

// CLINT (Core Local Interruptor) Implementation
void rv_clint_init(rv_clint* clint) {
    memset(clint, 0, sizeof(*clint));
}

u8 rv_clint_read8(rv_t* rv, u64 addr) {
    return (u8)rv_clint_read32(rv, addr & ~3);
}

u16 rv_clint_read16(rv_t* rv, u64 addr) {
    return (u16)rv_clint_read32(rv, addr & ~3);
}

u32 rv_clint_read32(rv_t* rv, u64 addr) {
    if (addr == 0x0)
        return rv->clint.mswi;
    else if (addr == 0x4000)
        return rv->clint.mtimecmp;
    else if (addr == 0x4004)
        return rv->clint.mtimecmph;
    else if (addr == 0xBFF8)
        return rv->csr.mtime & 0xffffffff;
    else if (addr == 0xBFFC)
        return rv->csr.mtime >> 32;
    return 0;
}

void rv_clint_write8(rv_t* rv, u64 addr, u8 data) {
    rv_clint_write32(rv, addr, data);
}

void rv_clint_write16(rv_t* rv, u64 addr, u16 data) {
    rv_clint_write32(rv, addr, data);
}

void rv_clint_write32(rv_t* rv, u64 addr, u32 data) {
    if (addr == 0x0)
        rv->clint.mswi = data;
    else if (addr == 0x4000)
        rv->clint.mtimecmp = data;
    else if (addr == 0x4004)
        rv->clint.mtimecmph = data;
    else if (addr == 0xBFF8)
        rv->csr.mtime = (rv->csr.mtime & 0xffffffff00000000ULL) | data;
    else if (addr == 0xBFFC)
        rv->csr.mtime = (rv->csr.mtime & 0xffffffffULL) | ((u64)data << 32);
}

u32 rv_clint_msi(rv_t* rv, u32 context) {
    (void)context;
    return rv->clint.mswi & 1;
}

u32 rv_clint_mti(rv_t* rv, u32 context) {
    (void)context;
    return (rv->csr.mtime >> 32 > rv->clint.mtimecmph) ||
           ((rv->csr.mtime >> 32 == rv->clint.mtimecmph) &&
            ((rv->csr.mtime & 0xffffffff) >= rv->clint.mtimecmp));
}

// PLIC (Platform-Level Interrupt Controller) Implementation
void rv_plic_init(rv_plic* plic) {
    memset(plic, 0, sizeof(*plic));
}

u8 rv_plic_read8(rv_t* rv, u64 addr) {
    return (u8)rv_plic_read32(rv, addr & ~3);
}

u16 rv_plic_read16(rv_t* rv, u64 addr) {
    return (u16)rv_plic_read32(rv, addr & ~3);
}

u32 rv_plic_read32(rv_t* rv, u64 addr) {
    if (addr < RV_PLIC_NSRC * 4)
        return rv->plic.priority[addr >> 2];
    else if (addr >= 0x1000 && addr < 0x1000 + RV_PLIC_NSRC / 8)
        return rv->plic.pending[(addr - 0x1000) >> 2];
    else if (addr >= 0x2000 && addr < 0x2000 + RV_PLIC_NSRC / 8)
        return rv->plic.enable[(addr - 0x2000) >> 2];
    else if (addr >> 12 >= 0x200 && (addr >> 12) < 0x200 + RV_PLIC_NCTX && !(addr & 0xFFF))
        return rv->plic.thresh[(addr >> 12) - 0x200];
    else if (addr >> 12 >= 0x200 && (addr >> 12) < 0x200 + RV_PLIC_NCTX && (addr & 0xFFF) == 4) {
        u32 context = (addr >> 12) - 0x200;
        u32 claim_reg = rv->plic.claim[context];
        if (claim_reg < RV_PLIC_NSRC) {
            u32 en_off = context * RV_PLIC_NSRC / 32;
            if (rv->plic.pending[claim_reg / 32] & (1U << claim_reg % 32))
                rv->plic.claiming[claim_reg / 32 + en_off] |= 1U << claim_reg % 32;
        }
        return claim_reg;
    }
    return 0;
}

void rv_plic_write8(rv_t* rv, u64 addr, u8 data) {
    rv_plic_write32(rv, addr, data);
}

void rv_plic_write16(rv_t* rv, u64 addr, u16 data) {
    rv_plic_write32(rv, addr, data);
}

void rv_plic_write32(rv_t* rv, u64 addr, u32 data) {
    if (addr < RV_PLIC_NSRC * 4 && addr != 0)
        rv->plic.priority[addr >> 2] = data;
    else if (addr >= 0x2000 && addr < 0x2000 + RV_PLIC_NSRC / 8 && addr != 0x2000)
        rv->plic.enable[(addr - 0x2000) >> 2] = data;
    else if (addr >> 12 >= 0x200 && (addr >> 12) < 0x200 + RV_PLIC_NCTX && !(addr & 0xFFF))
        rv->plic.thresh[(addr >> 12) - 0x200] = data;
    else if (addr >> 12 >= 0x200 && (addr >> 12) < 0x200 + RV_PLIC_NCTX && (addr & 0xFFF) == 4) {
        u32 context = (addr >> 12) - 0x200;
        if (data < RV_PLIC_NSRC) {
            u32 en_off = context * RV_PLIC_NSRC / 32;
            rv->plic.claiming[data / 32 + en_off] &= ~(1U << data % 32);
        }
    }
}

rv_res_e rv_plic_irq(rv_t* rv, u32 source) {
    if (source > RV_PLIC_NSRC || !source ||
        ((rv->plic.claiming[source / 32] >> (source % 32)) & 1U) ||
        ((rv->plic.pending[source / 32] >> (source % 32)) & 1U))
        return RV_RES_BAD;
    rv->plic.pending[source / 32] |= 1U << source % 32;
    return RV_RES_OK;
}

u32 rv_plic_mei(rv_t* rv, u32 context) {
    u32 i, j, o = 0, h = 0;
    for (i = 0; i < RV_PLIC_NSRC / 32; i++) {
        u32 en_off = i + context * RV_PLIC_NSRC / 32;
        if (!((rv->plic.enable[en_off] & rv->plic.pending[i]) | rv->plic.claiming[i]))
            continue;
        for (j = 0; j < 32; j++) {
            if ((rv->plic.claiming[en_off] >> j) & 1U)
                rv->plic.pending[i] &= ~(1U << j);
            else if (((rv->plic.enable[i] >> j) & 1U) &&
                     ((rv->plic.pending[i] >> j) & 1U) &&
                     rv->plic.priority[i * 32 + j] >= h &&
                     rv->plic.priority[i * 32 + j] >= rv->plic.thresh[context])
                o = i * 32 + j, h = rv->plic.priority[i * 32 + j];
        }
    }
    rv->plic.claim[context] = o;
    return !!o;
}

// UART Implementation
void rv_uart_init(rv_uart* uart) {
    memset(uart, 0, sizeof(*uart));
    uart->tx_ready = 1;
}

u8 rv_uart_read8(rv_t* rv, u64 addr) {
    return (u8)rv_uart_read32(rv, addr);
}

u16 rv_uart_read16(rv_t* rv, u64 addr) {
    return (u16)rv_uart_read32(rv, addr);
}

u32 rv_uart_read32(rv_t* rv, u64 addr) {
    switch (addr) {
        case 0x00:
            if (rv->uart.rx_ready) {
                rv->uart.rx_ready = 0;
                return rv->uart.rx_data;
            }
            return 0;
        case 0x14:
            return (rv->uart.tx_ready << 5) | (rv->uart.rx_ready << 0);
        default:
            return 0;
    }
}

void rv_uart_write8(rv_t* rv, u64 addr, u8 data) {
    rv_uart_write32(rv, addr, data);
}

void rv_uart_write16(rv_t* rv, u64 addr, u16 data) {
    rv_uart_write32(rv, addr, data);
}

void rv_uart_write32(rv_t* rv, u64 addr, u32 data) {
    switch (addr) {
        case 0x00:
            if (rv->uart.tx_ready && (u8)data != '\r') {
                putchar((u8)data);
                fflush(stdout);
            }
            break;
        default:
            break;
    }
}

// Machine-level memory access functions
u8 rv_machine_read8(rv_t* rv, u64 addr) {
    if (addr >= MACH_RAM_BASE && addr < MACH_RAM_BASE + MACH_RAM_SIZE) {
        return rv_read8(rv, addr - MACH_RAM_BASE);
    }
    else if (addr >= MACH_CLINT0_BASE && addr < MACH_CLINT0_BASE + RV_CLINT_SIZE) {
        return rv_clint_read8(rv, addr - MACH_CLINT0_BASE);
    }
    else if (addr >= MACH_PLIC0_BASE && addr < MACH_PLIC0_BASE + RV_PLIC_SIZE) {
        return rv_plic_read8(rv, addr - MACH_PLIC0_BASE);
    }
    else if (addr >= MACH_UART0_BASE && addr < MACH_UART0_BASE + 0x1000) {
        return rv_uart_read8(rv, addr - MACH_UART0_BASE);
    }
    return 0;
}

u16 rv_machine_read16(rv_t* rv, u64 addr) {
    if (addr >= MACH_RAM_BASE && addr < MACH_RAM_BASE + MACH_RAM_SIZE) {
        return rv_read16(rv, addr - MACH_RAM_BASE);
    }
    else if (addr >= MACH_CLINT0_BASE && addr < MACH_CLINT0_BASE + RV_CLINT_SIZE) {
        return rv_clint_read16(rv, addr - MACH_CLINT0_BASE);
    }
    else if (addr >= MACH_PLIC0_BASE && addr < MACH_PLIC0_BASE + RV_PLIC_SIZE) {
        return rv_plic_read16(rv, addr - MACH_PLIC0_BASE);
    }
    else if (addr >= MACH_UART0_BASE && addr < MACH_UART0_BASE + 0x1000) {
        return rv_uart_read16(rv, addr - MACH_UART0_BASE);
    }
    return 0;
}

u32 rv_machine_read32(rv_t* rv, u64 addr) {
    if (addr >= MACH_RAM_BASE && addr < MACH_RAM_BASE + MACH_RAM_SIZE) {
        u32 offset = addr - MACH_RAM_BASE;
        return rv_read32(rv, offset);
    }
    else if (addr >= MACH_CLINT0_BASE && addr < MACH_CLINT0_BASE + RV_CLINT_SIZE) {
        return rv_clint_read32(rv, addr - MACH_CLINT0_BASE);
    }
    else if (addr >= MACH_PLIC0_BASE && addr < MACH_PLIC0_BASE + RV_PLIC_SIZE) {
        return rv_plic_read32(rv, addr - MACH_PLIC0_BASE);
    }
    else if (addr >= MACH_UART0_BASE && addr < MACH_UART0_BASE + 0x1000) {
        return rv_uart_read32(rv, addr - MACH_UART0_BASE);
    }
    return 0;
}

u64 rv_machine_read64(rv_t* rv, u64 addr) {
    if (addr >= MACH_RAM_BASE && addr < MACH_RAM_BASE + MACH_RAM_SIZE) {
        return rv_read64(rv, addr - MACH_RAM_BASE);
    }
    // Other devices don't support 64-bit reads
    return 0;
}

void rv_machine_write8(rv_t* rv, u64 addr, u8 data) {
    if (addr >= MACH_RAM_BASE && addr < MACH_RAM_BASE + MACH_RAM_SIZE) {
        rv_write8(rv, addr - MACH_RAM_BASE, data);
    }
    else if (addr >= MACH_CLINT0_BASE && addr < MACH_CLINT0_BASE + RV_CLINT_SIZE) {
        rv_clint_write8(rv, addr - MACH_CLINT0_BASE, data);
    }
    else if (addr >= MACH_PLIC0_BASE && addr < MACH_PLIC0_BASE + RV_PLIC_SIZE) {
        rv_plic_write8(rv, addr - MACH_PLIC0_BASE, data);
    }
    else if (addr >= MACH_UART0_BASE && addr < MACH_UART0_BASE + 0x1000) {
        rv_uart_write8(rv, addr - MACH_UART0_BASE, data);
    }
}

void rv_machine_write16(rv_t* rv, u64 addr, u16 data) {
    if (addr >= MACH_RAM_BASE && addr < MACH_RAM_BASE + MACH_RAM_SIZE) {
        rv_write16(rv, addr - MACH_RAM_BASE, data);
    }
    else if (addr >= MACH_CLINT0_BASE && addr < MACH_CLINT0_BASE + RV_CLINT_SIZE) {
        rv_clint_write16(rv, addr - MACH_CLINT0_BASE, data);
    }
    else if (addr >= MACH_PLIC0_BASE && addr < MACH_PLIC0_BASE + RV_PLIC_SIZE) {
        rv_plic_write16(rv, addr - MACH_PLIC0_BASE, data);
    }
    else if (addr >= MACH_UART0_BASE && addr < MACH_UART0_BASE + 0x1000) {
        rv_uart_write16(rv, addr - MACH_UART0_BASE, data);
    }
}

void rv_machine_write32(rv_t* rv, u64 addr, u32 data) {
    if (addr >= MACH_RAM_BASE && addr < MACH_RAM_BASE + MACH_RAM_SIZE) {
        rv_write32(rv, addr - MACH_RAM_BASE, data);
    }
    else if (addr >= MACH_CLINT0_BASE && addr < MACH_CLINT0_BASE + RV_CLINT_SIZE) {
        rv_clint_write32(rv, addr - MACH_CLINT0_BASE, data);
    }
    else if (addr >= MACH_PLIC0_BASE && addr < MACH_PLIC0_BASE + RV_PLIC_SIZE) {
        rv_plic_write32(rv, addr - MACH_PLIC0_BASE, data);
    }
    else if (addr >= MACH_UART0_BASE && addr < MACH_UART0_BASE + 0x1000) {
        rv_uart_write32(rv, addr - MACH_UART0_BASE, data);
    }
}

void rv_machine_write64(rv_t* rv, u64 addr, u64 data) {
    if (addr >= MACH_RAM_BASE && addr < MACH_RAM_BASE + MACH_RAM_SIZE) {
        rv_write64(rv, addr - MACH_RAM_BASE, data);
    }
    // Other devices don't support 64-bit writes
}
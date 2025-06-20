#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "rv64gc.h"

// Bitfield extraction macros (needed for CSR and trap handling)
#define BITS(x, hi, lo) (((x) >> (lo)) & ((1ULL << ((hi) - (lo) + 1)) - 1))
#define BIT(x, n) BITS(x, n, n)

void rv_init(rv_t *rv) {
  memset(rv, 0, sizeof(rv_t));
  rv->ram = malloc(MACH_RAM_SIZE);
  rv->pc = RV_RESET_VEC;
  rv->priv = RV_PRIV_MACHINE;
  rv->csr.misa = (3ULL << 62) |  // RV64 (MXL=3)
                 (1ULL << ('A' - 'A')) |
                 (1ULL << ('C' - 'A')) |
                 (1ULL << ('D' - 'A')) |
                 (1ULL << ('F' - 'A')) |
                 (1ULL << ('I' - 'A')) |
                 (1ULL << ('M' - 'A')) |
                 (1ULL << ('S' - 'A')) |
                 (1ULL << ('U' - 'A'));
  rv_clint_init(&rv->clint);
  rv_plic_init(&rv->plic);
  rv_uart_init(&rv->uart);
}

err_e rv_free(rv_t *rv) {
  if (!rv) return ERROR;
  if (!rv->ram) return ERROR;
  free(rv->ram);
  rv->ram = NULL;
  return OK;
}

u8 rv_read8(rv_t* rv, u64 addr) {
    if (addr >= MACH_RAM_SIZE) {
        return 0;
    }
    return rv->ram[addr];
}

u16 rv_read16(rv_t* rv, u64 addr) {
    if (addr + 1 >= MACH_RAM_SIZE) {
        return 0;
    }
    return *(u16*)&rv->ram[addr];
}

u32 rv_read32(rv_t* rv, u64 addr) {
    if (addr + 3 >= MACH_RAM_SIZE) {
        return 0;
    }
    return *(u32*)&rv->ram[addr];
}

u64 rv_read64(rv_t* rv, u64 addr) {
    if (addr + 7 >= MACH_RAM_SIZE) {
        return 0;
    }
    return *(u64*)&rv->ram[addr];
}

void rv_write8(rv_t* rv, u64 addr, u8 data) {
    if (addr >= MACH_RAM_SIZE) {
        return;
    }
    rv->ram[addr] = data;
}

void rv_write16(rv_t* rv, u64 addr, u16 data) {
    if (addr + 1 >= MACH_RAM_SIZE) {
        return;
    }
    *(u16*)&rv->ram[addr] = data;
}

void rv_write32(rv_t* rv, u64 addr, u32 data) {
    if (addr + 3 >= MACH_RAM_SIZE) {
        return;
    }
    *(u32*)&rv->ram[addr] = data;
}

void rv_write64(rv_t* rv, u64 addr, u64 data) {
    if (addr + 7 >= MACH_RAM_SIZE) {
        return;
    }
    *(u64*)&rv->ram[addr] = data;
}

// CSR access functions
u64 rv_csr_read(rv_t* rv, u32 csr_addr) {
    switch(csr_addr) {
        // Supervisor CSRs (0x100 sstatus maps to mstatus with mask)
        case 0x100: return rv->csr.mstatus & 0x800DE762;
        case 0x104: return rv->csr.sie;
        case 0x105: return rv->csr.stvec;
        case 0x106: return rv->csr.scounteren;
        case 0x140: return rv->csr.sscratch;
        case 0x141: return rv->csr.sepc;
        case 0x142: return rv->csr.scause;
        case 0x143: return rv->csr.stval;
        case 0x144: return rv->csr.sip;
        case 0x180: return rv->csr.satp;
        
        // Machine Information Registers
        case 0xF11: return rv->csr.mvendorid;
        case 0xF12: return rv->csr.marchid;
        case 0xF13: return rv->csr.mimpid;
        case 0xF14: return rv->csr.mhartid;
        
        // Machine Trap Setup
        case 0x300: return rv->csr.mstatus;
        case 0x301: return rv->csr.misa;
        case 0x302: return rv->csr.medeleg;
        case 0x303: return rv->csr.mideleg;
        case 0x304: return rv->csr.mie;
        case 0x305: return rv->csr.mtvec;
        case 0x306: return rv->csr.mcounteren;
        case 0x310: return rv->csr.mstatush;
        
        // Machine Trap Handling
        case 0x340: return rv->csr.mscratch;
        case 0x341: return rv->csr.mepc;
        case 0x342: return rv->csr.mcause;
        case 0x343: return rv->csr.mtval;
        case 0x344: return rv->csr.mip;
        case 0x320: return rv->csr.mcountinhibit;
        
        // Machine Counters
        case 0xB00: return rv->csr.mcycle & 0xffffffff;
        case 0xB80: return rv->csr.mcycle >> 32;
        case 0xB02: return rv->csr.minstret & 0xffffffff;
        case 0xB82: return rv->csr.minstret >> 32;
        
        // Time
        case 0xC01: return rv->csr.mtime & 0xffffffff;
        case 0xC81: return rv->csr.mtime >> 32;
        
        // Unprivileged counters (aliases)
        case 0xC00: return rv->csr.mcycle & 0xffffffff;  // cycle
        case 0xC80: return rv->csr.mcycle >> 32;         // cycleh
        case 0xC02: return rv->csr.minstret & 0xffffffff; // instret
        case 0xC82: return rv->csr.minstret >> 32;       // instreth
        
        default:
            if (csr_addr >= 0x3A0 && csr_addr <= 0x3A3)
                return rv->csr.pmpcfg[csr_addr - 0x3A0];
            if (csr_addr >= 0x3B0 && csr_addr <= 0x3BF)
                return rv->csr.pmpaddr[csr_addr - 0x3B0];
            return 0;
    }
}

void rv_csr_write(rv_t* rv, u32 csr_addr, u64 value) {
    switch(csr_addr) {
        // Supervisor CSRs 
        case 0x100: // sstatus - write to mstatus with mask
            rv->csr.mstatus = (rv->csr.mstatus & ~0x800DE762) | (value & 0x800DE762);
            break;
        case 0x104: rv->csr.sie = value & 0x222; break;
        case 0x105: rv->csr.stvec = value; break;
        case 0x106: rv->csr.scounteren = value; break;
        case 0x140: rv->csr.sscratch = value; break;
        case 0x141: rv->csr.sepc = value; break;
        case 0x142: rv->csr.scause = value; break;
        case 0x143: rv->csr.stval = value; break;
        case 0x144: rv->csr.sip = value & 0x222; break;
        case 0x180: rv->csr.satp = value; break;
        
        // Machine Trap Setup
        case 0x300: rv->csr.mstatus = value; break;
        case 0x301: rv->csr.misa = value; break;
        case 0x302: rv->csr.medeleg = value; break;
        case 0x303: rv->csr.mideleg = value; break;
        case 0x304: rv->csr.mie = value; break;
        case 0x305: rv->csr.mtvec = value; break;
        case 0x306: rv->csr.mcounteren = value; break;
        case 0x310: rv->csr.mstatush = value & 0x30; break;
        
        // Machine Trap Handling
        case 0x340: rv->csr.mscratch = value; break;
        case 0x341: rv->csr.mepc = value; break;
        case 0x342: rv->csr.mcause = value; break;
        case 0x343: rv->csr.mtval = value; break;
        case 0x344: rv->csr.mip = value; break;
        case 0x320: rv->csr.mcountinhibit = value; break;
        
        // Machine Counters
        case 0xB00: rv->csr.mcycle = (rv->csr.mcycle & 0xffffffff00000000ULL) | value; break;
        case 0xB80: rv->csr.mcycle = (rv->csr.mcycle & 0xffffffffULL) | ((u64)value << 32); break;
        case 0xB02: rv->csr.minstret = (rv->csr.minstret & 0xffffffff00000000ULL) | value; break;
        case 0xB82: rv->csr.minstret = (rv->csr.minstret & 0xffffffffULL) | ((u64)value << 32); break;
        
        default:
            if (csr_addr >= 0x3A0 && csr_addr <= 0x3A3)
                rv->csr.pmpcfg[csr_addr - 0x3A0] = value;
            else if (csr_addr >= 0x3B0 && csr_addr <= 0x3BF)
                rv->csr.pmpaddr[csr_addr - 0x3B0] = value;
            break;
    }
}

void rv_endcvt(u8 *in, u8 *out, u32 width, u32 is_store) {
    if (!is_store && width == 1)
        *out = in[0];
    else if (!is_store && width == 2)
        *((u16 *)out) = (u16)(in[0] << 0) | (u16)(in[1] << 8);
    else if (!is_store && width == 4)
        *((u32 *)out) = (u32)(in[0] << 0) | (u32)(in[1] << 8) |
                       (u32)(in[2] << 16) | (u32)(in[3] << 24);
    else if (width == 1)
        out[0] = *in;
    else if (width == 2)
        out[0] = *(u16 *)in >> 0 & 0xFF, out[1] = (*(u16 *)in >> 8);
    else
        out[0] = *(u32 *)in >> 0 & 0xFF, out[1] = *(u32 *)in >> 8 & 0xFF,
        out[2] = *(u32 *)in >> 16 & 0xFF, out[3] = *(u32 *)in >> 24 & 0xFF;
}

rv_res_e rv_trap(rv_t* rv, u64 cause, u64 tval) {
    u64 is_interrupt = !!(cause & 0x8000000000000000ULL), rcause = cause & ~0x8000000000000000ULL;
    rv_priv_e xp = (rv->priv < RV_PRIV_MACHINE) &&
                   ((is_interrupt ? rv->csr.mideleg : rv->csr.medeleg) &
                    (1ULL << rcause))
                       ? RV_PRIV_SUPERVISOR
                       : RV_PRIV_MACHINE;
    u64 *xtvec = &rv->csr.mtvec, *xepc = &rv->csr.mepc,
        *xcause = &rv->csr.mcause, *xtval = &rv->csr.mtval;
    u64 xie = BIT(rv->csr.mstatus, xp);
    
    if (xp == RV_PRIV_SUPERVISOR)
        xtvec = &rv->csr.stvec, xepc = &rv->csr.sepc, xcause = &rv->csr.scause,
        xtval = &rv->csr.stval;
        
    rv->csr.mstatus &= (xp == RV_PRIV_MACHINE ? ~0x1888ULL : ~0x122ULL);
    rv->csr.mstatus |= (rv->priv << (xp == RV_PRIV_MACHINE ? 11 : 8)) |
                       xie << (4 + xp);
    *xepc = rv->pc;
    *xcause = rcause | (is_interrupt << 63);
    *xtval = tval;
    rv->priv = xp;
    rv->pc = (*xtvec & ~3ULL) + 4 * rcause * ((*xtvec & 1) && is_interrupt);
    return RV_RES_OK;  // Trap handled, continue execution
}

// Main CPU step function
rv_res_e rv_step(rv_t* rv) {
    // Fetch instruction
    u32 inst_raw;
    rv_res_e res = rv_vread32(rv, rv->pc, &inst_raw);
    
    
    if (res != RV_RES_OK) {
        if (res == RV_RES_PAGEFAULT) {
            return rv_trap(rv, RV_EIPAGE, rv->pc);
        }
        return rv_trap(rv, RV_EIFAULT, rv->pc);
    }
    
    // Decode instruction
    rv_inst_t inst = rv64_decode(inst_raw);
    
    // Check for illegal instruction (all zeros)
    if (inst_raw == 0) {
        return rv_trap(rv, RV_EILL, rv->pc);
    }
    
    // Update next PC based on instruction length
    // Compressed instructions have bits 0-1 != 11
    if ((inst_raw & 0x3) != 0x3) {
        rv->next_pc = rv->pc + 2;  // Compressed instruction
    } else {
        rv->next_pc = rv->pc + 4;  // Regular instruction
    }
    
    // Execute instruction
    res = rv64_interp(rv, inst);
    if (res != RV_RES_OK) {
        if (res == RV_RES_PAGEFAULT) {
            // Determine page fault type based on last operation
            // This is simplified - a real implementation would track the operation type
            return rv_trap(rv, RV_ELPAGE, rv->pc);
        } else if (res == RV_RES_BAD_ALIGN) {
            return rv_trap(rv, RV_ELALIGN, rv->pc);
        } else if (res == RV_RES_TRAP_WFI) {
            return RV_RES_TRAP_WFI;
        }
        return res;
    }
    
    // Update PC
    rv->pc = rv->next_pc;
    
    // Increment cycle counter
    rv->csr.mcycle++;
    rv->csr.minstret++;
    
    // Ensure x0 is always zero
    rv->x[0] = 0;
    
    return RV_RES_OK;
}

rv_res_e rv_vmm(rv_t* rv, u64 va, u64 *pa, rv_access_e access) {
    u32 epriv = BIT(rv->csr.mstatus, 17) && access != RV_ACCESS_EXEC
                    ? BITS(rv->csr.mstatus, 12, 11)
                    : rv->priv;
    // Check if translation is enabled (Sv39 mode = 8 in satp[63:60])
    if ((rv->csr.satp >> 60) != 8 || epriv > RV_PRIV_SUPERVISOR) {
        *pa = va;
    } else {
        // Sv39 translation
        if (va == rv->tlb_va && BIT(rv->tlb_valid, (access >> 1))) {
            *pa = rv->tlb_i;
            return RV_RES_OK;
        }
        u64 ppn = rv->csr.satp & 0x0FFFFFFFFFFFFFFFULL; // 44-bit PPN
        u64 pte = 0, i = 0;
        
        // Sv39 has 3 levels of page tables
        for (u32 j = 2; j >= 0; j--) {
            i = (ppn << 12) + ((va >> (j * 9 + 12)) & 0x1ff) * 8;
            pte = rv_machine_read64(rv, i);
            if (!(pte & 1) || (!(pte & 2) && (pte & 0x1c))) return RV_RES_PAGEFAULT;
            else if (pte & 0xe) break;
            ppn = (pte >> 10) & 0x0FFFFFFFFFFFFFFFULL;
        }
        
        // Calculate page mask based on level
        u64 mask = (1ULL << ((BITS(pte, 3, 1) == 7 ? 2 : BITS(pte, 3, 1) == 3 ? 1 : 0) * 9 + 12)) - 1;
        ppn = (pte >> 10) << 12;
        
        if ((pte & 0x8) || !((access & RV_ACCESS_WRITE) && !(pte & 0x4))) {
            if ((ppn & mask) != 0) return RV_RES_PAGEFAULT;
            rv->tlb_va = va & ~mask, rv->tlb_i = i = (ppn & ~mask) | (va & mask),
            rv->tlb_valid = ((pte >> 1) & 7) | !!BIT(pte, 4) << 1;
        } else
            i = ppn | (va & 0xfff);
        *pa = i;
    }
    return RV_RES_OK;
}

rv_res_e rv_vread8(rv_t* rv, u64 va, u8 *data) {
    u64 pa;
    rv_res_e err = rv_vmm(rv, va, &pa, RV_ACCESS_READ);
    if (err) return err;
    
    // In usermode, directly access RAM at low addresses
    if (rv->syscall_ctx && pa < MACH_RAM_SIZE) {
        *data = rv_read8(rv, pa);
    } else {
        *data = rv_machine_read8(rv, pa);
    }
    return RV_RES_OK;
}

rv_res_e rv_vread16(rv_t* rv, u64 va, u16 *data) {
    u64 pa;
    rv_res_e err = rv_vmm(rv, va, &pa, RV_ACCESS_READ);
    if (err) return err;
    
    // In usermode, directly access RAM at low addresses
    if (rv->syscall_ctx && pa < MACH_RAM_SIZE) {
        *data = rv_read16(rv, pa);
    } else {
        *data = rv_machine_read16(rv, pa);
    }
    return RV_RES_OK;
}

rv_res_e rv_vread32(rv_t* rv, u64 va, u32 *data) {
    u64 pa;
    rv_res_e err = rv_vmm(rv, va, &pa, RV_ACCESS_READ);
    if (err) return err;
    
    // In usermode, directly access RAM at low addresses
    if (rv->syscall_ctx && pa < MACH_RAM_SIZE) {
        *data = rv_read32(rv, pa);
    } else {
        *data = rv_machine_read32(rv, pa);
    }
    
    // Debug for usermode - disabled
    // if (rv->syscall_ctx && va < 0x20000) {
    //     fprintf(stderr, "vread32: va=%llx pa=%llx data=%08x\n", 
    //             (unsigned long long)va, (unsigned long long)pa, *data);
    // }
    
    return RV_RES_OK;
}

rv_res_e rv_vread64(rv_t* rv, u64 va, u64 *data) {
    u64 pa;
    rv_res_e err = rv_vmm(rv, va, &pa, RV_ACCESS_READ);
    if (err) return err;
    
    // In usermode, directly access RAM at low addresses
    if (rv->syscall_ctx && pa < MACH_RAM_SIZE) {
        *data = rv_read64(rv, pa);
    } else {
        *data = rv_machine_read64(rv, pa);
    }
    return RV_RES_OK;
}

rv_res_e rv_vwrite8(rv_t* rv, u64 va, u8 data) {
    u64 pa;
    rv_res_e err = rv_vmm(rv, va, &pa, RV_ACCESS_WRITE);
    if (err) return err;
    
    // In usermode, directly access RAM at low addresses
    if (rv->syscall_ctx && pa < MACH_RAM_SIZE) {
        rv_write8(rv, pa, data);
    } else {
        rv_machine_write8(rv, pa, data);
    }
    return RV_RES_OK;
}

rv_res_e rv_vwrite16(rv_t* rv, u64 va, u16 data) {
    u64 pa;
    rv_res_e err = rv_vmm(rv, va, &pa, RV_ACCESS_WRITE);
    if (err) return err;
    
    // In usermode, directly access RAM at low addresses
    if (rv->syscall_ctx && pa < MACH_RAM_SIZE) {
        rv_write16(rv, pa, data);
    } else {
        rv_machine_write16(rv, pa, data);
    }
    return RV_RES_OK;
}

rv_res_e rv_vwrite32(rv_t* rv, u64 va, u32 data) {
    u64 pa;
    rv_res_e err = rv_vmm(rv, va, &pa, RV_ACCESS_WRITE);
    if (err) return err;
    
    // In usermode, directly access RAM at low addresses
    if (rv->syscall_ctx && pa < MACH_RAM_SIZE) {
        rv_write32(rv, pa, data);
    } else {
        rv_machine_write32(rv, pa, data);
    }
    return RV_RES_OK;
}

rv_res_e rv_vwrite64(rv_t* rv, u64 va, u64 data) {
    u64 pa;
    rv_res_e err = rv_vmm(rv, va, &pa, RV_ACCESS_WRITE);
    if (err) return err;
    
    // In usermode, directly access RAM at low addresses
    if (rv->syscall_ctx && pa < MACH_RAM_SIZE) {
        rv_write64(rv, pa, data);
    } else {
        rv_machine_write64(rv, pa, data);
    }
    return RV_RES_OK;
}

void rv_irq(rv_t* rv, u64 cause) {
    rv->csr.mip &= ~(u32)(8 | 128 | 512);
    rv->csr.mip |= cause;
}

static u32 rv_service_interrupts(rv_t* rv) {
    u32 iidx, d;
    for (iidx = 12; iidx > 0; iidx--) {
        if (!(rv->csr.mip & rv->csr.mie & (1 << iidx)))
            continue;
        d = (rv->csr.mideleg & (1 << iidx)) ? RV_PRIV_SUPERVISOR : RV_PRIV_MACHINE;
        if (d == rv->priv ? BIT(rv->csr.mstatus, d) : (d > rv->priv))
            return rv_trap(rv, 0x80000000U + iidx, rv->pc);
    }
    return RV_RES_TRAP_NONE;
}

void rv_machine_init(rv_t* rv) {
    rv_init(rv);
}

void rv_machine_load_file(const char* path, u8* buf, u64 max_size) {
    FILE *f = fopen(path, "rb");
    if (!f) {
        printf("Unable to load file %s\n", path);
        exit(EXIT_FAILURE);
    }
    
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);
    
    if (size > max_size) {
        printf("File %s too large (%ld bytes, max %llu)\n", path, size, (unsigned long long)max_size);
        exit(EXIT_FAILURE);
    }
    
    size_t read_bytes = fread(buf, 1, size, f);
    printf("Loaded %s: %zu bytes\n", path, read_bytes);
    fclose(f);
}

u64 rv_machine_run(rv_t* rv, u64 max_cycles) {
    u64 cycle = 0;
    
    while (!max_cycles || cycle < max_cycles) {
        if ((cycle & 0xFFF) == 0) {
            if (!++rv->csr.mtime) {
            }
        }
        
        u32 irq = 0;
        if (rv_clint_msi(rv, 0)) irq |= 8;
        if (rv_clint_mti(rv, 0)) irq |= 128;
        if (rv_plic_mei(rv, 0)) irq |= 512;
        
        rv_irq(rv, irq);
        
        rv_res_e result = rv_step(rv);
        if (result == RV_RES_TRAP_WFI) {
            continue;
        } else if (result != RV_RES_OK) {
            printf("Emulation stopped with error: %d at PC: 0x%016llx\n", result, (unsigned long long)rv->pc);
            return result;
        }
        
        cycle++;
        
        if ((cycle & 0xFFFFF) == 0) {
            printf("Executed %llu instructions\n", (unsigned long long)cycle);
        }
    }
    
    return RV_RES_TRAP_NONE;
}
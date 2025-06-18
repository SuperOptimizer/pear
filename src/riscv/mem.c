#include "mem.h"
#include "rv32gc.h"
#include <math.h>

u8 rv_phys_read8(rv_t *rv, u32 paddr) {
  if (paddr >= MEMORY_SIZE) return 0;
  return rv->ram[paddr];
}

u16 rv_phys_read16(rv_t *rv, u32 paddr) {
  if (paddr + 1 >= MEMORY_SIZE) return 0;
  return *(u16 *) (rv->ram + paddr);
}

u32 rv_phys_read32(rv_t *rv, u32 paddr) {
  if (paddr + 3 >= MEMORY_SIZE) return 0;
  return *(u32 *) (rv->ram + paddr);
}

void rv_phys_write8(rv_t *rv, u32 paddr, u8 val) {
  if (paddr < MEMORY_SIZE) rv->ram[paddr] = val;
}

void rv_phys_write16(rv_t *rv, u32 paddr, u16 val) {
  if (paddr + 1 < MEMORY_SIZE) *(u16 *) (rv->ram + paddr) = val;
}

void rv_phys_write32(rv_t *rv, u32 paddr, u32 val) {
  if (paddr + 3 < MEMORY_SIZE) *(u32 *) (rv->ram + paddr) = val;
}

// Get effective privilege level for memory access
static u32 get_effective_priv(rv_t *rv) {
  // Check MPRV bit in machine mode
  if (rv->csr.priv == PRIV_MACHINE && (rv->csr.mstatus & MSTATUS_MPRV)) {
    return (rv->csr.mstatus & MSTATUS_MPP) >> 11;
  }
  return rv->csr.priv;
}

// Virtual memory translation for Sv32
int rv_translate_address(rv_t *rv, u32 vaddr, u32* paddr, int is_write, int is_exec) {
  // If SATP.MODE is 0, no translation
  if (!(rv->csr.satp & SATP_MODE)) {
    *paddr = vaddr;
    return 1; // Success
  }

  u32 priv = get_effective_priv(rv);
  u32 vpn1 = (vaddr >> 22) & 0x3FF;
  u32 vpn0 = (vaddr >> 12) & 0x3FF;
  u32 offset = vaddr & 0xFFF;

  // Get page table base
  u32 pt_base = (rv->csr.satp & SATP_PPN) << 12;

  // Level 1 page table lookup
  u32 pte1_addr = pt_base + vpn1 * 4;
  u32 pte1 = rv_phys_read32(rv, pte1_addr);

  if (!(pte1 & PTE_V)) {
    return 0; // Page fault
  }

  // Check if this is a leaf PTE (megapage)
  if (pte1 & (PTE_R | PTE_W | PTE_X)) {
    // Megapage - check alignment
    if ((pte1 >> 10) & 0x3FF) {
      return 0; // Misaligned megapage
    }

    u32 ppn = (pte1 >> 10) & 0xFFFFF;
    *paddr = (ppn << 12) | (vaddr & 0x3FFFFF);

    // Check permissions
    if (is_exec && !(pte1 & PTE_X)) return 0;
    if (is_write && !(pte1 & PTE_W)) return 0;
    if (!is_exec && !is_write && !(pte1 & PTE_R)) return 0;

    // Check user/supervisor access
    if (priv == PRIV_USER && !(pte1 & PTE_U)) return 0;
    if (priv != PRIV_USER && (pte1 & PTE_U) && !(rv->csr.mstatus & MSTATUS_SUM)) return 0;

    return 1;
  }

  // Level 0 page table lookup
  u32 pt0_base = ((pte1 >> 10) & 0xFFFFF) << 12;
  u32 pte0_addr = pt0_base + vpn0 * 4;
  u32 pte0 = rv_phys_read32(rv, pte0_addr);

  if (!(pte0 & PTE_V)) {
    return 0; // Page fault
  }

  if (!(pte0 & (PTE_R | PTE_W | PTE_X))) {
    return 0; // Non-leaf PTE at level 0
  }

  u32 ppn = (pte0 >> 10) & 0xFFFFF;
  *paddr = (ppn << 12) | offset;

  // Check permissions (same as above)
  if (is_exec && !(pte0 & PTE_X)) return 0;
  if (is_write && !(pte0 & PTE_W)) return 0;
  if (!is_exec && !is_write && !(pte0 & PTE_R)) return 0;

  if (priv == PRIV_USER && !(pte0 & PTE_U)) return 0;
  if (priv != PRIV_USER && (pte0 & PTE_U) && !(rv->csr.mstatus & MSTATUS_SUM)) return 0;

  return 1;
}

u8 rv_mem_read8(rv_t *rv, u32 addr) {
  u32 paddr;
  if (!rv_translate_address(rv, addr, &paddr, 0, 0)) {
    rv_raise_exception(rv, CAUSE_LOAD_PAGE_FAULT, addr);
    return 0;
  }
  return rv_phys_read8(rv, paddr);
}

u16 rv_mem_read16(rv_t *rv, u32 addr) {
  // Check alignment
  if (addr & 1) {
    rv_raise_exception(rv, CAUSE_MISALIGNED_LOAD, addr);
    return 0;
  }

  u32 paddr;
  if (!rv_translate_address(rv, addr, &paddr, 0, 0)) {
    rv_raise_exception(rv, CAUSE_LOAD_PAGE_FAULT, addr);
    return 0;
  }
  return rv_phys_read16(rv, paddr);
}

u32 rv_mem_read32(rv_t *rv, u32 addr) {
  // Check alignment
  if (addr & 3) {
    rv_raise_exception(rv, CAUSE_MISALIGNED_LOAD, addr);
    return 0;
  }

  u32 paddr;
  if (!rv_translate_address(rv, addr, &paddr, 0, 0)) {
    rv_raise_exception(rv, CAUSE_LOAD_PAGE_FAULT, addr);
    return 0;
  }
  return rv_phys_read32(rv, paddr);
}

void rv_mem_write8(rv_t *rv, u32 addr, u8 val) {
  u32 paddr;
  if (!rv_translate_address(rv, addr, &paddr, 1, 0)) {
    rv_raise_exception(rv, CAUSE_STORE_PAGE_FAULT, addr);
    return;
  }
  rv_phys_write8(rv, paddr, val);
}

void rv_mem_write16(rv_t *rv, u32 addr, u16 val) {
  // Check alignment
  if (addr & 1) {
    rv_raise_exception(rv, CAUSE_MISALIGNED_STORE, addr);
    return;
  }

  u32 paddr;
  if (!rv_translate_address(rv, addr, &paddr, 1, 0)) {
    rv_raise_exception(rv, CAUSE_STORE_PAGE_FAULT, addr);
    return;
  }
  rv_phys_write16(rv, paddr, val);
}

void rv_mem_write32(rv_t *rv, u32 addr, u32 val) {
  // Check alignment
  if (addr & 3) {
    rv_raise_exception(rv, CAUSE_MISALIGNED_STORE, addr);
    return;
  }

  u32 paddr;
  if (!rv_translate_address(rv, addr, &paddr, 1, 0)) {
    rv_raise_exception(rv, CAUSE_STORE_PAGE_FAULT, addr);
    return;
  }
  rv_phys_write32(rv, paddr, val);
}

// Instruction fetch with translation
u32 rv_fetch_instruction(rv_t *rv, u32 addr) {
  // Check alignment - RISC-V instructions must be 2-byte aligned (for compressed instructions)
  if (addr & 1) {
    rv_raise_exception(rv, CAUSE_MISALIGNED_FETCH, addr);
    return 0;
  }

  u32 paddr;
  if (!rv_translate_address(rv, addr, &paddr, 0, 1)) {
    rv_raise_exception(rv, CAUSE_FETCH_PAGE_FAULT, addr);
    return 0;
  }
  
  // Read first 16 bits to determine instruction type
  u16 first_half = rv_phys_read16(rv, paddr);
  
  // Check if it's a compressed instruction (bottom 2 bits != 11)
  if ((first_half & 0x3) != 0x3) {
    // Compressed instruction - return as lower 16 bits of 32-bit word
    return (u32)first_half;
  } else {
    // Full 32-bit instruction - read second half
    u16 second_half = rv_phys_read16(rv, paddr + 2);
    return ((u32)second_half << 16) | first_half;
  }
}

// Double-precision memory operations
f64 rv_mem_read_f64(rv_t *rv, u32 addr) {
  union {
    f64 f;
    u64 i;
  } u;
  u.i = ((u64) rv_mem_read32(rv, addr + 4) << 32) | rv_mem_read32(rv, addr);
  return u.f;
}

void rv_mem_write_f64(rv_t *rv, u32 addr, f64 val) {
  union {
    f64 f;
    u64 i;
  } u = {.f = val};
  rv_mem_write32(rv, addr, u.i & 0xFFFFFFFF);
  rv_mem_write32(rv, addr + 4, u.i >> 32);
}
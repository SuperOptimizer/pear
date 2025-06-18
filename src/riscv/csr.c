#include "csr.h"
#include "rv32gc.h"
#include <stdio.h>

// Initialize CSR state
void rv_csr_init(rv_t *rv) {
  memset(&rv->csr, 0, sizeof(csr_state_t));

  // Set up machine ISA
  rv->csr.misa = (1 << 30) | // RV32
                 (1 << 8) | // I - Base integer ISA
                 (1 << 12) | // M - Integer multiply/divide
                 (1 << 0) | // A - Atomic extension
                 (1 << 5) | // F - Single-precision floating-point
                 (1 << 3) | // D - Double-precision floating-point
                 (1 << 2) | // C - Compressed extension
                 (1 << 18) | // S - Supervisor mode
                 (1 << 20); // U - User mode

  // Set machine info
  rv->csr.mvendorid = 0; // Non-commercial implementation
  rv->csr.marchid = 0;
  rv->csr.mimpid = 0;
  rv->csr.mhartid = 0;

  // Start in machine mode
  rv->csr.priv = PRIV_MACHINE;
  rv->csr.mstatus = 0;
}

// Exception handling
void rv_raise_exception(rv_t *rv, u32 cause, u32 tval) {
  u32 deleg_bit = 1 << cause;
  u32 target_priv = PRIV_MACHINE;

  // Check delegation
  if (rv->csr.priv <= PRIV_SUPERVISOR && (rv->csr.medeleg & deleg_bit)) {
    target_priv = PRIV_SUPERVISOR;
  }

  if (target_priv == PRIV_MACHINE) {
    // Machine mode exception
    rv->csr.mepc = rv->pc;
    rv->csr.mcause = cause;
    rv->csr.mtval = tval;

    u32 mpp = rv->csr.priv;
    rv->csr.mstatus = (rv->csr.mstatus & ~MSTATUS_MPP) | (mpp << 11);
    rv->csr.mstatus = (rv->csr.mstatus & ~MSTATUS_MPIE) |
                      ((rv->csr.mstatus & MSTATUS_MIE) ? MSTATUS_MPIE : 0);
    rv->csr.mstatus &= ~MSTATUS_MIE;

    rv->csr.priv = PRIV_MACHINE;
    rv->pc = rv->csr.mtvec & ~3;

    // Vectored interrupts
    if ((rv->csr.mtvec & 3) == 1 && (cause & 0x80000000)) {
      rv->pc += (cause & 0x7FFFFFFF) * 4;
    }
  } else {
    // Supervisor mode exception
    rv->csr.sepc = rv->pc;
    rv->csr.scause = cause;
    rv->csr.stval = tval;

    u32 spp = (rv->csr.priv == PRIV_SUPERVISOR) ? (1 << 8) : 0;
    rv->csr.sstatus = (rv->csr.sstatus & ~MSTATUS_SPP) | spp;
    rv->csr.sstatus = (rv->csr.sstatus & ~MSTATUS_SPIE) |
                      ((rv->csr.sstatus & MSTATUS_SIE) ? MSTATUS_SPIE : 0);
    rv->csr.sstatus &= ~MSTATUS_SIE;

    rv->csr.priv = PRIV_SUPERVISOR;
    rv->pc = rv->csr.stvec & ~3;

    if ((rv->csr.stvec & 3) == 1 && (cause & 0x80000000)) {
      rv->pc += (cause & 0x7FFFFFFF) * 4;
    }
  }
}

// Interrupt handling
void rv_check_interrupts(rv_t *rv) {
  u32 pending = rv->csr.mip & rv->csr.mie;

  if (!pending) return;

  u32 enabled_interrupts = 0;

  // Check interrupt enable based on privilege level
  switch (rv->csr.priv) {
    case PRIV_MACHINE:
      if (rv->csr.mstatus & MSTATUS_MIE) enabled_interrupts = pending;
      break;
    case PRIV_SUPERVISOR:
      enabled_interrupts = pending & ~rv->csr.mideleg;
      if (rv->csr.mstatus & MSTATUS_SIE) {
        enabled_interrupts |= pending & rv->csr.mideleg;
      }
      break;
    case PRIV_USER:
      enabled_interrupts = pending & ~rv->csr.mideleg;
      if (rv->csr.mstatus & MSTATUS_SIE) {
        enabled_interrupts |= pending & rv->csr.mideleg & ~rv->csr.sideleg;
      }
      if (rv->csr.mstatus & MSTATUS_UIE) {
        enabled_interrupts |= pending & rv->csr.mideleg & rv->csr.sideleg;
      }
      break;
  }

  if (!enabled_interrupts) return;

  // Priority order: MEI, MSI, MTI, SEI, SSI, STI, UEI, USI, UTI
  u32 interrupt_cause = 0;
  if (enabled_interrupts & MIP_MEIP) interrupt_cause = 11;
  else if (enabled_interrupts & MIP_MSIP) interrupt_cause = 3;
  else if (enabled_interrupts & MIP_MTIP) interrupt_cause = 7;
  else if (enabled_interrupts & MIP_SEIP) interrupt_cause = 9;
  else if (enabled_interrupts & MIP_SSIP) interrupt_cause = 1;
  else if (enabled_interrupts & MIP_STIP) interrupt_cause = 5;
  else if (enabled_interrupts & MIP_UEIP) interrupt_cause = 8;
  else if (enabled_interrupts & MIP_USIP) interrupt_cause = 0;
  else if (enabled_interrupts & MIP_UTIP) interrupt_cause = 4;

  if (interrupt_cause) {
    rv_raise_exception(rv, 0x80000000 | interrupt_cause, 0);
  }
}

// CSR read implementation
u32 rv_csr_read(rv_t *rv, u32 csr) {
  // Check privilege level
  u32 min_priv = (csr >> 8) & 3;
  if (rv->csr.priv < min_priv) {
    rv_raise_exception(rv, CAUSE_ILLEGAL_INSTRUCTION, 0);
    return 0;
  }

  switch (csr) {
    // User counters
    case CSR_CYCLE: return rv->csr.cycle & 0xFFFFFFFF;
    case CSR_TIME: return rv->csr.time & 0xFFFFFFFF;
    case CSR_INSTRET: return rv->csr.instret & 0xFFFFFFFF;
    case CSR_CYCLEH: return rv->csr.cycle >> 32;
    case CSR_TIMEH: return rv->csr.time >> 32;
    case CSR_INSTRETH: return rv->csr.instret >> 32;

    // Supervisor CSRs
    case CSR_SSTATUS: return rv->csr.sstatus;
    case CSR_SIE: return rv->csr.sie;
    case CSR_STVEC: return rv->csr.stvec;
    case CSR_SCOUNTEREN: return rv->csr.scounteren;
    case CSR_SSCRATCH: return rv->csr.sscratch;
    case CSR_SEPC: return rv->csr.sepc;
    case CSR_SCAUSE: return rv->csr.scause;
    case CSR_STVAL: return rv->csr.stval;
    case CSR_SIP: return rv->csr.sip;
    case CSR_SATP: return rv->csr.satp;

    // Machine CSRs
    case CSR_MSTATUS: return rv->csr.mstatus;
    case CSR_MISA: return rv->csr.misa;
    case CSR_MEDELEG: return rv->csr.medeleg;
    case CSR_MIDELEG: return rv->csr.mideleg;
    case CSR_MIE: return rv->csr.mie;
    case CSR_MTVEC: return rv->csr.mtvec;
    case CSR_MCOUNTEREN: return rv->csr.mcounteren;
    case CSR_MSCRATCH: return rv->csr.mscratch;
    case CSR_MEPC: return rv->csr.mepc;
    case CSR_MCAUSE: return rv->csr.mcause;
    case CSR_MTVAL: return rv->csr.mtval;
    case CSR_MIP: return rv->csr.mip;
    case CSR_MCYCLE: return rv->csr.cycle & 0xFFFFFFFF;
    case CSR_MINSTRET: return rv->csr.instret & 0xFFFFFFFF;
    case CSR_MCYCLEH: return rv->csr.cycle >> 32;
    case CSR_MINSTRETH: return rv->csr.instret >> 32;
    case CSR_MVENDORID: return rv->csr.mvendorid;
    case CSR_MARCHID: return rv->csr.marchid;
    case CSR_MIMPID: return rv->csr.mimpid;
    case CSR_MHARTID: return rv->csr.mhartid;

    default:
      rv_raise_exception(rv, CAUSE_ILLEGAL_INSTRUCTION, 0);
      return 0;
  }
}

// CSR write implementation
void rv_csr_write(rv_t *rv, u32 csr, u32 val) {
  // Check privilege level
  u32 min_priv = (csr >> 8) & 3;
  if (rv->csr.priv < min_priv) {
    rv_raise_exception(rv, CAUSE_ILLEGAL_INSTRUCTION, 0);
    return;
  }

  // Check read-only
  if ((csr >> 10) == 3) {
    rv_raise_exception(rv, CAUSE_ILLEGAL_INSTRUCTION, 0);
    return;
  }

  switch (csr) {
    // Supervisor CSRs
    case CSR_SSTATUS:
      rv->csr.sstatus = val & 0x800DE133; // Mask for valid bits
      rv->csr.mstatus = (rv->csr.mstatus & ~0x800DE133) | rv->csr.sstatus;
      break;
    case CSR_SIE:
      rv->csr.sie = val & 0x333;
      rv->csr.mie = (rv->csr.mie & ~0x333) | rv->csr.sie;
      break;
    case CSR_STVEC: rv->csr.stvec = val;
      break;
    case CSR_SCOUNTEREN: rv->csr.scounteren = val;
      break;
    case CSR_SSCRATCH: rv->csr.sscratch = val;
      break;
    case CSR_SEPC: rv->csr.sepc = val & ~1;
      break;
    case CSR_SCAUSE: rv->csr.scause = val;
      break;
    case CSR_STVAL: rv->csr.stval = val;
      break;
    case CSR_SIP:
      rv->csr.sip = val & 0x333;
      rv->csr.mip = (rv->csr.mip & ~0x333) | rv->csr.sip;
      break;
    case CSR_SATP: rv->csr.satp = val;
      break;

    // Machine CSRs
    case CSR_MSTATUS:
      rv->csr.mstatus = val & 0x807FFFFF; // Mask for valid bits
      rv->csr.sstatus = rv->csr.mstatus & 0x800DE133;
      break;
    case CSR_MEDELEG: rv->csr.medeleg = val & 0xF0B7;
      break;
    case CSR_MIDELEG: rv->csr.mideleg = val & 0x333;
      break;
    case CSR_MIE:
      rv->csr.mie = val & 0xFFF;
      rv->csr.sie = rv->csr.mie & 0x333;
      break;
    case CSR_MTVEC: rv->csr.mtvec = val;
      break;
    case CSR_MCOUNTEREN: rv->csr.mcounteren = val;
      break;
    case CSR_MSCRATCH: rv->csr.mscratch = val;
      break;
    case CSR_MEPC: rv->csr.mepc = val & ~1;
      break;
    case CSR_MCAUSE: rv->csr.mcause = val;
      break;
    case CSR_MTVAL: rv->csr.mtval = val;
      break;
    case CSR_MIP:
      rv->csr.mip = (rv->csr.mip & 0x777) | (val & 0x888);
      rv->csr.sip = rv->csr.mip & 0x333;
      break;
    case CSR_MCYCLE: rv->csr.cycle = (rv->csr.cycle & 0xFFFFFFFF00000000ULL) | val;
      break;
    case CSR_MINSTRET: rv->csr.instret = (rv->csr.instret & 0xFFFFFFFF00000000ULL) | val;
      break;
    case CSR_MCYCLEH: rv->csr.cycle = (rv->csr.cycle & 0xFFFFFFFF) | ((u64) val << 32);
      break;
    case CSR_MINSTRETH: rv->csr.instret = (rv->csr.instret & 0xFFFFFFFF) | ((u64) val << 32);
      break;

    default:
      rv_raise_exception(rv, CAUSE_ILLEGAL_INSTRUCTION, 0);
      break;
  }
}

// MRET/SRET instruction implementations
void rv_mret(rv_t *rv) {
  if (rv->csr.priv < PRIV_MACHINE) {
    rv_raise_exception(rv, CAUSE_ILLEGAL_INSTRUCTION, 0);
    return;
  }

  u32 mpp = (rv->csr.mstatus & MSTATUS_MPP) >> 11;
  rv->csr.priv = mpp;

  if ((rv->csr.mstatus & MSTATUS_MPIE)) {
    rv->csr.mstatus |= MSTATUS_MIE;
  } else {
    rv->csr.mstatus &= ~MSTATUS_MIE;
  }

  rv->csr.mstatus |= MSTATUS_MPIE;
  rv->csr.mstatus &= ~MSTATUS_MPP;

  rv->pc = rv->csr.mepc - 4; // -4 because rv_interpret() will add 4
}

void rv_sret(rv_t *rv) {
  if (rv->csr.priv < PRIV_SUPERVISOR) {
    rv_raise_exception(rv, CAUSE_ILLEGAL_INSTRUCTION, 0);
    return;
  }

  if (rv->csr.priv == PRIV_SUPERVISOR && (rv->csr.mstatus & MSTATUS_TSR)) {
    rv_raise_exception(rv, CAUSE_ILLEGAL_INSTRUCTION, 0);
    return;
  }

  u32 spp = (rv->csr.sstatus & MSTATUS_SPP) ? PRIV_SUPERVISOR : PRIV_USER;
  rv->csr.priv = spp;

  if ((rv->csr.sstatus & MSTATUS_SPIE)) {
    rv->csr.sstatus |= MSTATUS_SIE;
  } else {
    rv->csr.sstatus &= ~MSTATUS_SIE;
  }

  rv->csr.sstatus |= MSTATUS_SPIE;
  rv->csr.sstatus &= ~MSTATUS_SPP;

  // Update mstatus to reflect sstatus changes
  rv->csr.mstatus = (rv->csr.mstatus & ~0x800DE133) | rv->csr.sstatus;

  rv->pc = rv->csr.sepc - 4; // -4 because rv_interpret() will add 4
}

// Other system instructions
void rv_handle_wfi(rv_t *rv) {
  // Wait for interrupt - in emulator, this is a no-op
  // Real implementation might halt until interrupt
}

void rv_handle_sfence_vma(rv_t *rv, u32 rs1, u32 rs2) {
  // Supervisor fence for virtual memory
  if (rv->csr.priv < PRIV_SUPERVISOR) {
    rv_raise_exception(rv, CAUSE_ILLEGAL_INSTRUCTION, 0);
    return;
  }

  if (rv->csr.priv == PRIV_SUPERVISOR && (rv->csr.mstatus & MSTATUS_TVM)) {
    rv_raise_exception(rv, CAUSE_ILLEGAL_INSTRUCTION, 0);
    return;
  }

  // Flush TLB entries - no-op in emulator since we don't cache translations
}

// Timer interrupt simulation
void rv_update_timer(rv_t *rv) {
  rv->csr.cycle++;
  rv->csr.instret++;
  rv->csr.time++;

  // Simple timer interrupt every 1000 cycles
  if (rv->csr.cycle % 1000 == 0) {
    rv->csr.mip |= MIP_MTIP;
  }
}

// External interrupt
void rv_set_external_interrupt(rv_t *rv, int enable) {
  if (enable) {
    rv->csr.mip |= MIP_MEIP;
  } else {
    rv->csr.mip &= ~MIP_MEIP;
  }
}

// System call handling
void rv_handle_syscall(rv_t *rv) {
  u32 syscall_num = rv->a0;

  switch (syscall_num) {
    case 93: // sys_exit
      printf("Process exited with code %d\n", rv->a1);
      break;

    case 64: // sys_write
    {
      u32 fd = rv->a1;
      u32 buf_addr = rv->a2;
      u32 count = rv->a3;

      if (fd == 1 || fd == 2) {
        // stdout or stderr
        for (u32 i = 0; i < count; i++) {
          u8 byte = rv_mem_read8(rv, buf_addr + i);
          putchar(byte);
        }
        rv->a0 = count; // Return bytes written
      } else {
        rv->a0 = -1; // Error
      }
    }
    break;

    case 63: // sys_read
      rv->a0 = 0; // EOF
      break;

    default:
      rv->a0 = -1; // Unimplemented
      break;
  }
}

// SBI (Supervisor Binary Interface) implementation
void rv_handle_sbi_call(rv_t *rv) {
  u32 sbi_ext = rv->a7; // Extension ID
  u32 sbi_func = rv->a6; // Function ID

  switch (sbi_ext) {
    case 0x01: // SBI_EXT_BASE
      switch (sbi_func) {
        case 0: // get_sbi_spec_version
          rv->a0 = 0x200; // SBI spec version 0.2
          rv->a1 = 0;
          break;
        case 1: // get_sbi_impl_id
          rv->a0 = 0; // Unknown implementation
          rv->a1 = 0;
          break;
        case 2: // get_sbi_impl_version
          rv->a0 = 0;
          rv->a1 = 0;
          break;
        default:
          rv->a0 = -2; // SBI_ERR_NOT_SUPPORTED
          rv->a1 = 0;
          break;
      }
      break;

    case 0x54494D45: // SBI_EXT_TIME ("TIME")
      switch (sbi_func) {
        case 0: // set_timer
          rv->csr.mip &= ~MIP_MTIP;
          rv->a0 = 0; // SBI_SUCCESS
          rv->a1 = 0;
          break;
        default:
          rv->a0 = -2; // SBI_ERR_NOT_SUPPORTED
          rv->a1 = 0;
          break;
      }
      break;

    case 0x735049: // Legacy console
      switch (sbi_func) {
        case 0: // console_putchar
          putchar(rv->a0 & 0xFF);
          rv->a0 = 0;
          rv->a1 = 0;
          break;
        case 1: // console_getchar
          rv->a0 = -1; // No character available
          rv->a1 = 0;
          break;
        default:
          rv->a0 = -2;
          rv->a1 = 0;
          break;
      }
      break;

    default:
      rv->a0 = -2; // SBI_ERR_NOT_SUPPORTED
      rv->a1 = 0;
      break;
  }
}

// System call entry point
void rv_syscall_entry(rv_t *rv, u32 cause) {
  switch (cause) {
    case CAUSE_USER_ECALL:
      rv_handle_syscall(rv);
      // Return from syscall by advancing past ECALL
      if (rv->csr.priv == PRIV_SUPERVISOR) {
        rv->csr.sepc += 4;
      } else {
        rv->csr.mepc += 4;
      }
      break;

    case CAUSE_SUPERVISOR_ECALL:
      rv_handle_sbi_call(rv);
      if (rv->csr.priv == PRIV_MACHINE) {
        rv->csr.mepc += 4;
      }
      break;

    case CAUSE_MACHINE_ECALL:
      rv->csr.mepc += 4;
      break;
  }
}
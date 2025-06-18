#pragma once

#include "../common.h"
#include "rv32gc.h"

// CSR initialization and access
void rv_csr_init(rv_t *rv);
u32 rv_csr_read(rv_t *rv, u32 csr);
void rv_csr_write(rv_t *rv, u32 csr, u32 val);

// Exception and interrupt handling
void rv_raise_exception(rv_t *rv, u32 cause, u32 tval);
void rv_check_interrupts(rv_t *rv);

// System instructions
void rv_mret(rv_t *rv);
void rv_sret(rv_t *rv);
void rv_handle_wfi(rv_t *rv);
void rv_handle_sfence_vma(rv_t *rv, u32 rs1, u32 rs2);

// Timer and interrupt control
void rv_update_timer(rv_t *rv);
void rv_set_external_interrupt(rv_t *rv, int enable);

// System call handling
void rv_handle_syscall(rv_t *rv);
void rv_handle_sbi_call(rv_t *rv);
void rv_syscall_entry(rv_t *rv, u32 cause);
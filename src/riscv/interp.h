#pragma once

#include "../common.h"

#include "rv32gc.h"


// Core interpreter functions
instr_e rv_decode(u32 opcode);
void rv_interpret(rv_t *rv, u32 opcode);
int rv_step(rv_t *rv);

// User mode support
void rv_setup_user_mode(rv_t *rv, u32 entry_point, u32 stack_top);
int rv_load_user_elf(rv_t *rv, const char *filename);
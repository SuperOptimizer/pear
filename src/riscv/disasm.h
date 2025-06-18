#pragma once

#include "../common.h"
#include "rv32gc.h"

// Function declarations
void rv_disasm(rv_t *rv, u32 opcode, char* disasm, u32 len);
void rv_print_instruction(rv_t* rv, u32 opcode, u32 pc);
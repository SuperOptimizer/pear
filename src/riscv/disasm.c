#include "disasm.h"
#include "rv32gc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>



const char *reg_abi_names[32] = {
  "zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2",
  "s0", "s1", "a0", "a1", "a2", "a3", "a4", "a5",
  "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7",
  "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6"
};

const char* regname(u32 num) {
  assert(num >= 0 && num < 32);
  return reg_abi_names[num];
}


void rv_disasm(rv_t *rv, u32 opcode, char* disasm, u32 len) {
  instr_e instr = rv_decode(opcode);

  // Extract common fields
  u32 rd = get_rd(opcode);
  u32 rs1 = get_rs1(opcode);
  u32 rs2 = get_rs2(opcode);
  s32 imm_i = get_i_imm(opcode);
  s32 imm_s = get_s_imm(opcode);
  s32 imm_b = get_b_imm(opcode);
  s32 imm_u = get_u_imm(opcode);
  s32 imm_j = get_j_imm(opcode);

  // For compressed instructions
  u16 c_inst = opcode & 0xFFFF;

  switch (instr) {
    // RV32I Base Integer Instructions
    case ADD:
      snprintf(disasm, len, "add %s, %s, %s", regname(rd), regname(rs1), regname(rs2));
      break;

    case ADDI:
      snprintf(disasm, len, "addi %s, %s, %d",
               regname(rd), regname(rs1), imm_i);
      break;

    case SUB:
      snprintf(disasm, len, "sub %s, %s, %s",
               regname(rd), regname(rs1), regname(rs2));
      break;

    case LUI:
      snprintf(disasm, len, "lui %s, 0x%x",
               regname(rd), (imm_u >> 12) & 0xFFFFF);
      break;

    case AUIPC:
      snprintf(disasm, len, "auipc %s, 0x%x",
               regname(rd), (imm_u >> 12) & 0xFFFFF);
      break;

    // Logical
    case XOR:
      snprintf(disasm, len, "xor %s, %s, %s",
               regname(rd), regname(rs1), regname(rs2));
      break;

    case XORI:
      snprintf(disasm, len, "xori %s, %s, %d",
               regname(rd), regname(rs1), imm_i);
      break;

    case OR:
      snprintf(disasm, len, "or %s, %s, %s",
               regname(rd), regname(rs1), regname(rs2));
      break;

    case ORI:
      snprintf(disasm, len, "ori %s, %s, %d",
               regname(rd), regname(rs1), imm_i);
      break;

    case AND:
      snprintf(disasm, len, "and %s, %s, %s",
               regname(rd), regname(rs1), regname(rs2));
      break;

    case ANDI:
      snprintf(disasm, len, "andi %s, %s, %d",
               regname(rd), regname(rs1), imm_i);
      break;

    // Shift
    case SLL:
      snprintf(disasm, len, "sll %s, %s, %s",
               regname(rd), regname(rs1), regname(rs2));
      break;

    case SLLI:
      snprintf(disasm, len, "slli %s, %s, %d",
               regname(rd), regname(rs1), imm_i & 0x1F);
      break;

    case SRL:
      snprintf(disasm, len, "srl %s, %s, %s",
               regname(rd), regname(rs1), regname(rs2));
      break;

    case SRLI:
      snprintf(disasm, len, "srli %s, %s, %d",
               regname(rd), regname(rs1), imm_i & 0x1F);
      break;

    case SRA:
      snprintf(disasm, len, "sra %s, %s, %s",
               regname(rd), regname(rs1), regname(rs2));
      break;

    case SRAI:
      snprintf(disasm, len, "srai %s, %s, %d",
               regname(rd), regname(rs1), imm_i & 0x1F);
      break;

    // Compare
    case SLT:
      snprintf(disasm, len, "slt %s, %s, %s",
               regname(rd), regname(rs1), regname(rs2));
      break;

    case SLTI:
      snprintf(disasm, len, "slti %s, %s, %d",
               regname(rd), regname(rs1), imm_i);
      break;

    case SLTU:
      snprintf(disasm, len, "sltu %s, %s, %s",
               regname(rd), regname(rs1), regname(rs2));
      break;

    case SLTIU:
      snprintf(disasm, len, "sltiu %s, %s, %d",
               regname(rd), regname(rs1), imm_i);
      break;

    // Branch
    case BEQ:
      snprintf(disasm, len, "beq %s, %s, 0x%x",
               regname(rs1), reg_abi_names[rs2], rv->pc + imm_b);
      break;

    case BNE:
      snprintf(disasm, len, "bne %s, %s, 0x%x",
               regname(rs1), reg_abi_names[rs2], rv->pc + imm_b);
      break;

    case BLT:
      snprintf(disasm, len, "blt %s, %s, 0x%x",
               regname(rs1), reg_abi_names[rs2], rv->pc + imm_b);
      break;

    case BGE:
      snprintf(disasm, len, "bge %s, %s, 0x%x",
               regname(rs1), reg_abi_names[rs2], rv->pc + imm_b);
      break;

    case BLTU:
      snprintf(disasm, len, "bltu %s, %s, 0x%x",
               regname(rs1), reg_abi_names[rs2], rv->pc + imm_b);
      break;

    case BGEU:
      snprintf(disasm, len, "bgeu %s, %s, 0x%x",
               regname(rs1), reg_abi_names[rs2], rv->pc + imm_b);
      break;

    // Jump
    case JAL:
      if (rd == 0) {
        snprintf(disasm, len, "j 0x%x", rv->pc + imm_j);
      } else {
        snprintf(disasm, len, "jal %s, 0x%x",
                 regname(rd), rv->pc + imm_j);
      }
      break;

    case JALR:
      if (rd == 0 && imm_i == 0) {
        snprintf(disasm, len, "jr %s", regname(rs1));
      } else if (rd == 1 && imm_i == 0) {
        snprintf(disasm, len, "jalr %s", regname(rs1));
      } else {
        snprintf(disasm, len, "jalr %s, %d(%s)",
                 regname(rd), imm_i, regname(rs1));
      }
      break;

    // Load
    case LB:
      snprintf(disasm, len, "lb %s, %d(%s)",
               regname(rd), imm_i, regname(rs1));
      break;

    case LH:
      snprintf(disasm, len, "lh %s, %d(%s)",
               regname(rd), imm_i, regname(rs1));
      break;

    case LW:
      snprintf(disasm, len, "lw %s, %d(%s)",
               regname(rd), imm_i, regname(rs1));
      break;

    case LBU:
      snprintf(disasm, len, "lbu %s, %d(%s)",
               regname(rd), imm_i, regname(rs1));
      break;

    case LHU:
      snprintf(disasm, len, "lhu %s, %d(%s)",
               regname(rd), imm_i, regname(rs1));
      break;

    // Store
    case SB:
      snprintf(disasm, len, "sb %s, %d(%s)",
               reg_abi_names[rs2], imm_s, regname(rs1));
      break;

    case SH:
      snprintf(disasm, len, "sh %s, %d(%s)",
               reg_abi_names[rs2], imm_s, regname(rs1));
      break;

    case SW:
      snprintf(disasm, len, "sw %s, %d(%s)",
               reg_abi_names[rs2], imm_s, regname(rs1));
      break;

    // System
    case ECALL:
      snprintf(disasm, len, "ecall");
      break;

    case EBREAK:
      snprintf(disasm, len, "ebreak");
      break;

    // CSR Instructions
    case CSRRW:
      snprintf(disasm, len, "csrrw %s, 0x%x, %s",
               regname(rd), imm_i & 0xFFF, regname(rs1));
      break;

    case CSRRS:
      snprintf(disasm, len, "csrrs %s, 0x%x, %s",
               regname(rd), imm_i & 0xFFF, regname(rs1));
      break;

    case CSRRC:
      snprintf(disasm, len, "csrrc %s, 0x%x, %s",
               regname(rd), imm_i & 0xFFF, regname(rs1));
      break;

    // Compressed Instructions (basic ones)
    case C_ADDI: {
      u32 c_rd = get_c_rd_rs1(c_inst);
      s32 c_imm = sign_extend(((c_inst >> 12) & 1) << 5 | ((c_inst >> 2) & 0x1F), 6);
      if (c_imm == 0) {
        snprintf(disasm, len, "c.nop");
      } else {
        snprintf(disasm, len, "c.addi %s, %d",
                 reg_abi_names[c_rd], c_imm);
      }
    }
    break;

    case C_LI: {
      u32 c_rd = get_c_rd_rs1(c_inst);
      s32 c_imm = sign_extend(((c_inst >> 12) & 1) << 5 | ((c_inst >> 2) & 0x1F), 6);
      snprintf(disasm, len, "c.li %s, %d",
               reg_abi_names[c_rd], c_imm);
    }
    break;

    case C_LW: {
      u32 c_rd = get_c_rd_prime(c_inst);
      u32 c_rs1 = get_c_rs1_prime(c_inst);
      u32 c_offset = ((c_inst >> 6) & 1) << 2 | ((c_inst >> 10) & 0x7) << 3 | ((c_inst >> 5) & 1) << 6;
      snprintf(disasm, len, "c.lw %s, %d(%s)",
               reg_abi_names[c_rd], c_offset, reg_abi_names[c_rs1]);
    }
    break;

    case C_SW: {
      u32 c_rs1 = get_c_rs1_prime(c_inst);
      u32 c_rs2 = get_c_rs2_prime(c_inst);
      u32 c_offset = ((c_inst >> 6) & 1) << 2 | ((c_inst >> 10) & 0x7) << 3 | ((c_inst >> 5) & 1) << 6;
      snprintf(disasm, len, "c.sw %s, %d(%s)",
               reg_abi_names[c_rs2], c_offset, reg_abi_names[c_rs1]);
    }
    break;

    case C_J: {
      s32 c_offset = sign_extend(((c_inst >> 12) & 1) << 11 | ((c_inst >> 8) & 1) << 4 |
                                 ((c_inst >> 9) & 0x3) << 8 | ((c_inst >> 6) & 1) << 7 |
                                 ((c_inst >> 7) & 1) << 6 | ((c_inst >> 2) & 1) << 5 |
                                 ((c_inst >> 11) & 1) << 1 | ((c_inst >> 3) & 0x7) << 2, 12);
      snprintf(disasm, len, "c.j 0x%x", rv->pc + c_offset);
    }
    break;

    // System Instructions
    case FENCE:
      snprintf(disasm, len, "fence");
      break;

    case FENCE_I:
      snprintf(disasm, len, "fence.i");
      break;

    case MRET:
      snprintf(disasm, len, "mret");
      break;

    case SRET:
      snprintf(disasm, len, "sret");
      break;

    case WFI:
      snprintf(disasm, len, "wfi");
      break;

    case SFENCE_VMA:
      snprintf(disasm, len, "sfence.vma %s, %s",
               regname(rs1), regname(rs2));
      break;

    // CSR Immediate Instructions
    case CSRRWI:
      snprintf(disasm, len, "csrrwi %s, 0x%x, %d",
               regname(rd), imm_i & 0xFFF, rs1);
      break;

    case CSRRSI:
      snprintf(disasm, len, "csrrsi %s, 0x%x, %d",
               regname(rd), imm_i & 0xFFF, rs1);
      break;

    case CSRRCI:
      snprintf(disasm, len, "csrrci %s, 0x%x, %d",
               regname(rd), imm_i & 0xFFF, rs1);
      break;

    // RV32M Instructions
    case MUL:
      snprintf(disasm, len, "mul %s, %s, %s",
               regname(rd), regname(rs1), regname(rs2));
      break;

    case MULH:
      snprintf(disasm, len, "mulh %s, %s, %s",
               regname(rd), regname(rs1), regname(rs2));
      break;

    case MULHSU:
      snprintf(disasm, len, "mulhsu %s, %s, %s",
               regname(rd), regname(rs1), regname(rs2));
      break;

    case MULHU:
      snprintf(disasm, len, "mulhu %s, %s, %s",
               regname(rd), regname(rs1), regname(rs2));
      break;

    case DIV:
      snprintf(disasm, len, "div %s, %s, %s",
               regname(rd), regname(rs1), regname(rs2));
      break;

    case DIVU:
      snprintf(disasm, len, "divu %s, %s, %s",
               regname(rd), regname(rs1), regname(rs2));
      break;

    case REM:
      snprintf(disasm, len, "rem %s, %s, %s",
               regname(rd), regname(rs1), regname(rs2));
      break;

    case REMU:
      snprintf(disasm, len, "remu %s, %s, %s",
               regname(rd), regname(rs1), regname(rs2));
      break;

    // RV32A Instructions
    case LR_W:
      snprintf(disasm, len, "lr.w %s, (%s)",
               regname(rd), regname(rs1));
      break;

    case SC_W:
      snprintf(disasm, len, "sc.w %s, %s, (%s)",
               regname(rd), reg_abi_names[rs2], regname(rs1));
      break;

    case AMOSWAP_W:
      snprintf(disasm, len, "amoswap.w %s, %s, (%s)",
               regname(rd), reg_abi_names[rs2], regname(rs1));
      break;

    case AMOADD_W:
      snprintf(disasm, len, "amoadd.w %s, %s, (%s)",
               regname(rd), reg_abi_names[rs2], regname(rs1));
      break;

    case AMOXOR_W:
      snprintf(disasm, len, "amoxor.w %s, %s, (%s)",
               regname(rd), reg_abi_names[rs2], regname(rs1));
      break;

    case AMOAND_W:
      snprintf(disasm, len, "amoand.w %s, %s, (%s)",
               regname(rd), reg_abi_names[rs2], regname(rs1));
      break;

    case AMOOR_W:
      snprintf(disasm, len, "amoor.w %s, %s, (%s)",
               regname(rd), reg_abi_names[rs2], regname(rs1));
      break;

    case AMOMIN_W:
      snprintf(disasm, len, "amomin.w %s, %s, (%s)",
               regname(rd), reg_abi_names[rs2], regname(rs1));
      break;

    case AMOMAX_W:
      snprintf(disasm, len, "amomax.w %s, %s, (%s)",
               regname(rd), reg_abi_names[rs2], regname(rs1));
      break;

    case AMOMINU_W:
      snprintf(disasm, len, "amominu.w %s, %s, (%s)",
               regname(rd), reg_abi_names[rs2], regname(rs1));
      break;

    case AMOMAXU_W:
      snprintf(disasm, len, "amomaxu.w %s, %s, (%s)",
               regname(rd), reg_abi_names[rs2], regname(rs1));
      break;

    // RV32F Instructions
    case FLW:
      snprintf(disasm, len, "flw f%d, %d(%s)",
               rd, imm_i, regname(rs1));
      break;

    case FSW:
      snprintf(disasm, len, "fsw f%d, %d(%s)",
               rs2, imm_s, regname(rs1));
      break;

    case FADD_S:
      snprintf(disasm, len, "fadd.s f%d, f%d, f%d",
               rd, rs1, rs2);
      break;

    case FSUB_S:
      snprintf(disasm, len, "fsub.s f%d, f%d, f%d",
               rd, rs1, rs2);
      break;

    case FMUL_S:
      snprintf(disasm, len, "fmul.s f%d, f%d, f%d",
               rd, rs1, rs2);
      break;

    case FDIV_S:
      snprintf(disasm, len, "fdiv.s f%d, f%d, f%d",
               rd, rs1, rs2);
      break;

    case FSQRT_S:
      snprintf(disasm, len, "fsqrt.s f%d, f%d",
               rd, rs1);
      break;

    case FSGNJ_S:
      snprintf(disasm, len, "fsgnj.s f%d, f%d, f%d",
               rd, rs1, rs2);
      break;

    case FSGNJN_S:
      snprintf(disasm, len, "fsgnjn.s f%d, f%d, f%d",
               rd, rs1, rs2);
      break;

    case FSGNJX_S:
      snprintf(disasm, len, "fsgnjx.s f%d, f%d, f%d",
               rd, rs1, rs2);
      break;

    case FMIN_S:
      snprintf(disasm, len, "fmin.s f%d, f%d, f%d",
               rd, rs1, rs2);
      break;

    case FMAX_S:
      snprintf(disasm, len, "fmax.s f%d, f%d, f%d",
               rd, rs1, rs2);
      break;

    case FCVT_W_S:
      snprintf(disasm, len, "fcvt.w.s %s, f%d",
               regname(rd), rs1);
      break;

    case FCVT_WU_S:
      snprintf(disasm, len, "fcvt.wu.s %s, f%d",
               regname(rd), rs1);
      break;

    case FCVT_S_W:
      snprintf(disasm, len, "fcvt.s.w f%d, %s",
               rd, regname(rs1));
      break;

    case FCVT_S_WU:
      snprintf(disasm, len, "fcvt.s.wu f%d, %s",
               rd, regname(rs1));
      break;

    case FMV_X_W:
      snprintf(disasm, len, "fmv.x.w %s, f%d",
               regname(rd), rs1);
      break;

    case FMV_W_X:
      snprintf(disasm, len, "fmv.w.x f%d, %s",
               rd, regname(rs1));
      break;

    case FEQ_S:
      snprintf(disasm, len, "feq.s %s, f%d, f%d",
               regname(rd), rs1, rs2);
      break;

    case FLT_S:
      snprintf(disasm, len, "flt.s %s, f%d, f%d",
               regname(rd), rs1, rs2);
      break;

    case FLE_S:
      snprintf(disasm, len, "fle.s %s, f%d, f%d",
               regname(rd), rs1, rs2);
      break;

    case FCLASS_S:
      snprintf(disasm, len, "fclass.s %s, f%d",
               regname(rd), rs1);
      break;

    case FMADD_S: {
      u32 rs3 = (opcode >> 27) & 0x1F;
      snprintf(disasm, len, "fmadd.s f%d, f%d, f%d, f%d",
               rd, rs1, rs2, rs3);
    }
    break;

    case FMSUB_S: {
      u32 rs3 = (opcode >> 27) & 0x1F;
      snprintf(disasm, len, "fmsub.s f%d, f%d, f%d, f%d",
               rd, rs1, rs2, rs3);
    }
    break;

    case FNMSUB_S: {
      u32 rs3 = (opcode >> 27) & 0x1F;
      snprintf(disasm, len, "fnmsub.s f%d, f%d, f%d, f%d",
               rd, rs1, rs2, rs3);
    }
    break;

    case FNMADD_S: {
      u32 rs3 = (opcode >> 27) & 0x1F;
      snprintf(disasm, len, "fnmadd.s f%d, f%d, f%d, f%d",
               rd, rs1, rs2, rs3);
    }
    break;

    // RV32D Instructions
    case FLD:
      snprintf(disasm, len, "fld f%d, %d(%s)",
               rd, imm_i, regname(rs1));
      break;

    case FSD:
      snprintf(disasm, len, "fsd f%d, %d(%s)",
               rs2, imm_s, regname(rs1));
      break;

    case FADD_D:
      snprintf(disasm, len, "fadd.d f%d, f%d, f%d",
               rd, rs1, rs2);
      break;

    case FSUB_D:
      snprintf(disasm, len, "fsub.d f%d, f%d, f%d",
               rd, rs1, rs2);
      break;

    case FMUL_D:
      snprintf(disasm, len, "fmul.d f%d, f%d, f%d",
               rd, rs1, rs2);
      break;

    case FDIV_D:
      snprintf(disasm, len, "fdiv.d f%d, f%d, f%d",
               rd, rs1, rs2);
      break;

    case FSQRT_D:
      snprintf(disasm, len, "fsqrt.d f%d, f%d",
               rd, rs1);
      break;

    case FSGNJ_D:
      snprintf(disasm, len, "fsgnj.d f%d, f%d, f%d",
               rd, rs1, rs2);
      break;

    case FSGNJN_D:
      snprintf(disasm, len, "fsgnjn.d f%d, f%d, f%d",
               rd, rs1, rs2);
      break;

    case FSGNJX_D:
      snprintf(disasm, len, "fsgnjx.d f%d, f%d, f%d",
               rd, rs1, rs2);
      break;

    case FMIN_D:
      snprintf(disasm, len, "fmin.d f%d, f%d, f%d",
               rd, rs1, rs2);
      break;

    case FMAX_D:
      snprintf(disasm, len, "fmax.d f%d, f%d, f%d",
               rd, rs1, rs2);
      break;

    case FCVT_S_D:
      snprintf(disasm, len, "fcvt.s.d f%d, f%d",
               rd, rs1);
      break;

    case FCVT_D_S:
      snprintf(disasm, len, "fcvt.d.s f%d, f%d",
               rd, rs1);
      break;

    case FCVT_W_D:
      snprintf(disasm, len, "fcvt.w.d %s, f%d",
               regname(rd), rs1);
      break;

    case FCVT_WU_D:
      snprintf(disasm, len, "fcvt.wu.d %s, f%d",
               regname(rd), rs1);
      break;

    case FCVT_D_W:
      snprintf(disasm, len, "fcvt.d.w f%d, %s",
               rd, regname(rs1));
      break;

    case FCVT_D_WU:
      snprintf(disasm, len, "fcvt.d.wu f%d, %s",
               rd, regname(rs1));
      break;

    case FEQ_D:
      snprintf(disasm, len, "feq.d %s, f%d, f%d",
               regname(rd), rs1, rs2);
      break;

    case FLT_D:
      snprintf(disasm, len, "flt.d %s, f%d, f%d",
               regname(rd), rs1, rs2);
      break;

    case FLE_D:
      snprintf(disasm, len, "fle.d %s, f%d, f%d",
               regname(rd), rs1, rs2);
      break;

    case FCLASS_D:
      snprintf(disasm, len, "fclass.d %s, f%d",
               regname(rd), rs1);
      break;

    case FMADD_D: {
      u32 rs3 = (opcode >> 27) & 0x1F;
      snprintf(disasm, len, "fmadd.d f%d, f%d, f%d, f%d",
               rd, rs1, rs2, rs3);
    }
    break;

    case FMSUB_D: {
      u32 rs3 = (opcode >> 27) & 0x1F;
      snprintf(disasm, len, "fmsub.d f%d, f%d, f%d, f%d",
               rd, rs1, rs2, rs3);
    }
    break;

    case FNMSUB_D: {
      u32 rs3 = (opcode >> 27) & 0x1F;
      snprintf(disasm, len, "fnmsub.d f%d, f%d, f%d, f%d",
               rd, rs1, rs2, rs3);
    }
    break;

    case FNMADD_D: {
      u32 rs3 = (opcode >> 27) & 0x1F;
      snprintf(disasm, len, "fnmadd.d f%d, f%d, f%d, f%d",
               rd, rs1, rs2, rs3);
    }
    break;

    // Additional Compressed Instructions
    case C_ADDI4SPN: {
      u32 c_rd = get_c_rd_prime(c_inst);
      u32 c_imm = ((c_inst >> 11) & 0x3) << 4 | ((c_inst >> 7) & 0xF) << 6 |
                  ((c_inst >> 6) & 1) << 2 | ((c_inst >> 5) & 1) << 3;
      snprintf(disasm, len, "c.addi4spn %s, %d",
               reg_abi_names[c_rd], c_imm);
    }
    break;

    case C_LWSP: {
      u32 c_rd = get_c_rd_rs1(c_inst);
      u32 c_offset = ((c_inst >> 4) & 0x7) << 2 | ((c_inst >> 12) & 1) << 5 | ((c_inst >> 2) & 0x3) << 6;
      snprintf(disasm, len, "c.lwsp %s, %d(sp)",
               reg_abi_names[c_rd], c_offset);
    }
    break;

    case C_SWSP: {
      u32 c_rs2 = get_c_rs2(c_inst);
      u32 c_offset = ((c_inst >> 9) & 0xF) << 2 | ((c_inst >> 7) & 0x3) << 6;
      snprintf(disasm, len, "c.swsp %s, %d(sp)",
               reg_abi_names[c_rs2], c_offset);
    }
    break;

    case C_JAL: {
      s32 c_offset = sign_extend(((c_inst >> 12) & 1) << 11 | ((c_inst >> 8) & 1) << 4 |
                                 ((c_inst >> 9) & 0x3) << 8 | ((c_inst >> 6) & 1) << 7 |
                                 ((c_inst >> 7) & 1) << 6 | ((c_inst >> 2) & 1) << 5 |
                                 ((c_inst >> 11) & 1) << 1 | ((c_inst >> 3) & 0x7) << 2, 12);
      snprintf(disasm, len, "c.jal 0x%x", rv->pc + c_offset);
    }
    break;

    case C_JR: {
      u32 c_rs1 = get_c_rd_rs1(c_inst);
      snprintf(disasm, len, "c.jr %s", reg_abi_names[c_rs1]);
    }
    break;

    case C_JALR: {
      u32 c_rs1 = get_c_rd_rs1(c_inst);
      snprintf(disasm, len, "c.jalr %s", reg_abi_names[c_rs1]);
    }
    break;

    case C_BEQZ: {
      u32 c_rs1 = get_c_rs1_prime(c_inst);
      s32 c_offset = sign_extend(((c_inst >> 12) & 1) << 8 | ((c_inst >> 6) & 0x3) << 3 |
                                 ((c_inst >> 5) & 1) << 7 | ((c_inst >> 2) & 1) << 6 |
                                 ((c_inst >> 10) & 0x3) << 1 | ((c_inst >> 3) & 0x3) << 5, 9);
      snprintf(disasm, len, "c.beqz %s, 0x%x",
               reg_abi_names[c_rs1], rv->pc + c_offset);
    }
    break;

    case C_BNEZ: {
      u32 c_rs1 = get_c_rs1_prime(c_inst);
      s32 c_offset = sign_extend(((c_inst >> 12) & 1) << 8 | ((c_inst >> 6) & 0x3) << 3 |
                                 ((c_inst >> 5) & 1) << 7 | ((c_inst >> 2) & 1) << 6 |
                                 ((c_inst >> 10) & 0x3) << 1 | ((c_inst >> 3) & 0x3) << 5, 9);
      snprintf(disasm, len, "c.bnez %s, 0x%x",
               reg_abi_names[c_rs1], rv->pc + c_offset);
    }
    break;

    case C_SLLI: {
      u32 c_rd = get_c_rd_rs1(c_inst);
      u32 c_shamt = ((c_inst >> 12) & 1) << 5 | ((c_inst >> 2) & 0x1F);
      snprintf(disasm, len, "c.slli %s, %d",
               reg_abi_names[c_rd], c_shamt);
    }
    break;

    case C_SRLI: {
      u32 c_rd = get_c_rs1_prime(c_inst);
      u32 c_shamt = ((c_inst >> 12) & 1) << 5 | ((c_inst >> 2) & 0x1F);
      snprintf(disasm, len, "c.srli %s, %d",
               reg_abi_names[c_rd], c_shamt);
    }
    break;

    case C_SRAI: {
      u32 c_rd = get_c_rs1_prime(c_inst);
      u32 c_shamt = ((c_inst >> 12) & 1) << 5 | ((c_inst >> 2) & 0x1F);
      snprintf(disasm, len, "c.srai %s, %d",
               reg_abi_names[c_rd], c_shamt);
    }
    break;

    case C_ANDI: {
      u32 c_rd = get_c_rs1_prime(c_inst);
      s32 c_imm = sign_extend(((c_inst >> 12) & 1) << 5 | ((c_inst >> 2) & 0x1F), 6);
      snprintf(disasm, len, "c.andi %s, %d",
               reg_abi_names[c_rd], c_imm);
    }
    break;

    case C_MV: {
      u32 c_rd = get_c_rd_rs1(c_inst);
      u32 c_rs2 = get_c_rs2(c_inst);
      snprintf(disasm, len, "c.mv %s, %s",
               reg_abi_names[c_rd], reg_abi_names[c_rs2]);
    }
    break;

    case C_ADD: {
      u32 c_rd = get_c_rd_rs1(c_inst);
      u32 c_rs2 = get_c_rs2(c_inst);
      snprintf(disasm, len, "c.add %s, %s",
               reg_abi_names[c_rd], reg_abi_names[c_rs2]);
    }
    break;

    case C_SUB: {
      u32 c_rd = get_c_rs1_prime(c_inst);
      u32 c_rs2 = get_c_rs2_prime(c_inst);
      snprintf(disasm, len, "c.sub %s, %s",
               reg_abi_names[c_rd], reg_abi_names[c_rs2]);
    }
    break;

    case C_XOR: {
      u32 c_rd = get_c_rs1_prime(c_inst);
      u32 c_rs2 = get_c_rs2_prime(c_inst);
      snprintf(disasm, len, "c.xor %s, %s",
               reg_abi_names[c_rd], reg_abi_names[c_rs2]);
    }
    break;

    case C_OR: {
      u32 c_rd = get_c_rs1_prime(c_inst);
      u32 c_rs2 = get_c_rs2_prime(c_inst);
      snprintf(disasm, len, "c.or %s, %s",
               reg_abi_names[c_rd], reg_abi_names[c_rs2]);
    }
    break;

    case C_AND: {
      u32 c_rd = get_c_rs1_prime(c_inst);
      u32 c_rs2 = get_c_rs2_prime(c_inst);
      snprintf(disasm, len, "c.and %s, %s",
               reg_abi_names[c_rd], reg_abi_names[c_rs2]);
    }
    break;

    case C_EBREAK:
      snprintf(disasm, len, "c.ebreak");
      break;

    // Compressed Floating-Point Instructions
    case C_FLW: {
      u32 c_rd = get_c_rd_prime(c_inst);
      u32 c_rs1 = get_c_rs1_prime(c_inst);
      u32 c_offset = ((c_inst >> 6) & 1) << 2 | ((c_inst >> 10) & 0x7) << 3 | ((c_inst >> 5) & 1) << 6;
      snprintf(disasm, len, "c.flw f%d, %d(%s)",
               c_rd, c_offset, reg_abi_names[c_rs1]);
    }
    break;

    case C_FSW: {
      u32 c_rs1 = get_c_rs1_prime(c_inst);
      u32 c_rs2 = get_c_rs2_prime(c_inst);
      u32 c_offset = ((c_inst >> 6) & 1) << 2 | ((c_inst >> 10) & 0x7) << 3 | ((c_inst >> 5) & 1) << 6;
      snprintf(disasm, len, "c.fsw f%d, %d(%s)",
               c_rs2, c_offset, reg_abi_names[c_rs1]);
    }
    break;

    case C_FLWSP: {
      u32 c_rd = get_c_rd_rs1(c_inst);
      u32 c_offset = ((c_inst >> 4) & 0x7) << 2 | ((c_inst >> 12) & 1) << 5 | ((c_inst >> 2) & 0x3) << 6;
      snprintf(disasm, len, "c.flwsp f%d, %d(sp)",
               c_rd, c_offset);
    }
    break;

    case C_FSWSP: {
      u32 c_rs2 = get_c_rs2(c_inst);
      u32 c_offset = ((c_inst >> 9) & 0xF) << 2 | ((c_inst >> 7) & 0x3) << 6;
      snprintf(disasm, len, "c.fswsp f%d, %d(sp)",
               c_rs2, c_offset);
    }
    break;

    case C_FLD: {
      u32 c_rd = get_c_rd_prime(c_inst);
      u32 c_rs1 = get_c_rs1_prime(c_inst);
      u32 c_offset = ((c_inst >> 10) & 0x7) << 3 | ((c_inst >> 5) & 0x3) << 6;
      snprintf(disasm, len, "c.fld f%d, %d(%s)",
               c_rd, c_offset, reg_abi_names[c_rs1]);
    }
    break;

    case C_FSD: {
      u32 c_rs1 = get_c_rs1_prime(c_inst);
      u32 c_rs2 = get_c_rs2_prime(c_inst);
      u32 c_offset = ((c_inst >> 10) & 0x7) << 3 | ((c_inst >> 5) & 0x3) << 6;
      snprintf(disasm, len, "c.fsd f%d, %d(%s)",
               c_rs2, c_offset, reg_abi_names[c_rs1]);
    }
    break;

    case C_FLDSP: {
      u32 c_rd = get_c_rd_rs1(c_inst);
      u32 c_offset = ((c_inst >> 5) & 0x3) << 3 | ((c_inst >> 12) & 1) << 5 | ((c_inst >> 2) & 0x7) << 6;
      snprintf(disasm, len, "c.fldsp f%d, %d(sp)",
               c_rd, c_offset);
    }
    break;

    case C_FSDSP: {
      u32 c_rs2 = get_c_rs2(c_inst);
      u32 c_offset = ((c_inst >> 10) & 0x7) << 3 | ((c_inst >> 7) & 0x7) << 6;
      snprintf(disasm, len, "c.fsdsp f%d, %d(sp)",
               c_rs2, c_offset);
    }
    break;

    case INSTR_INVALID:
    default:
      snprintf(disasm, len, "unknown (0x%08x)", opcode);
      break;
  }
}

void rv_print_instruction(rv_t *rv, u32 opcode, u32 pc) {
  char disasm[256];
  rv_disasm(rv, opcode, disasm, sizeof(disasm));
  printf("0x%08x: %08x  %s\n", pc, opcode, disasm);
}

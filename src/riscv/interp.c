#include "interp.h"
#include "rv32gc.h"
#include "../elf.h"
#include <math.h>

// Atomic reservation functions (simplified for emulator)
static void rv_set_reservation(rv_t *rv, u32 addr) {
  rv->reservation_addr = addr;
  rv->reservation_valid = 1;
}

static int rv_check_reservation(rv_t *rv, u32 addr) {
  return rv->reservation_valid && rv->reservation_addr == addr;
}

static void rv_clear_reservation(rv_t *rv) {
  rv->reservation_valid = 0;
}

instr_e rv_decode(u32 opcode) {
  u32 op = opcode & 0x7F;
  u32 funct3 = (opcode >> 12) & 0x7;
  u32 funct7 = (opcode >> 25) & 0x7F;
  u32 funct5 = (opcode >> 27) & 0x1F;

  switch (op) {
    case 0x37:
      return LUI;

    case 0x17:
      return AUIPC;

    case 0x6F:
      return JAL;

    case 0x67:
      if (funct3 == 0x0) return JALR;
      break;

    case 0x63:
      switch (funct3) {
        case 0x0: return BEQ;
        case 0x1: return BNE;
        case 0x4: return BLT;
        case 0x5: return BGE;
        case 0x6: return BLTU;
        case 0x7: return BGEU;
      }
      break;

    case 0x03:
      switch (funct3) {
        case 0x0: return LB;
        case 0x1: return LH;
        case 0x2: return LW;
        case 0x4: return LBU;
        case 0x5: return LHU;
      }
      break;

    case 0x23:
      switch (funct3) {
        case 0x0: return SB;
        case 0x1: return SH;
        case 0x2: return SW;
      }
      break;

    case 0x13:
      switch (funct3) {
        case 0x0: return ADDI;
        case 0x2: return SLTI;
        case 0x3: return SLTIU;
        case 0x4: return XORI;
        case 0x6: return ORI;
        case 0x7: return ANDI;
        case 0x1: return SLLI;
        case 0x5:
          if (funct7 == 0x00) return SRLI;
          if (funct7 == 0x20) return SRAI;
          break;
      }
      break;

    case 0x33:
      if (funct7 == 0x01) {
        switch (funct3) {
          case 0x0: return MUL;
          case 0x1: return MULH;
          case 0x2: return MULHSU;
          case 0x3: return MULHU;
          case 0x4: return DIV;
          case 0x5: return DIVU;
          case 0x6: return REM;
          case 0x7: return REMU;
        }
      } else if (funct7 == 0x00 || funct7 == 0x20) {
        switch (funct3) {
          case 0x0:
            if (funct7 == 0x00) return ADD;
            if (funct7 == 0x20) return SUB;
            break;
          case 0x1: return SLL;
          case 0x2: return SLT;
          case 0x3: return SLTU;
          case 0x4: return XOR;
          case 0x5:
            if (funct7 == 0x00) return SRL;
            if (funct7 == 0x20) return SRA;
            break;
          case 0x6: return OR;
          case 0x7: return AND;
        }
      }
      break;

    case 0x0F:
      if (funct3 == 0x0) return FENCE;
      if (funct3 == 0x1) return FENCE_I;
      break;

    case 0x73:
      if (opcode == 0x00000073) return ECALL;
      if (opcode == 0x00100073) return EBREAK;

      switch (funct3) {
        case 0x1: return CSRRW;
        case 0x2: return CSRRS;
        case 0x3: return CSRRC;
        case 0x5: return CSRRWI;
        case 0x6: return CSRRSI;
        case 0x7: return CSRRCI;
      }
      break;

    case 0x2F:
      switch (funct5) {
        case 0x02: return LR_W;
        case 0x03: return SC_W;
        case 0x01: return AMOSWAP_W;
        case 0x00: return AMOADD_W;
        case 0x04: return AMOXOR_W;
        case 0x0C: return AMOAND_W;
        case 0x08: return AMOOR_W;
        case 0x10: return AMOMIN_W;
        case 0x14: return AMOMAX_W;
        case 0x18: return AMOMINU_W;
        case 0x1C: return AMOMAXU_W;
      }
      break;

    case 0x07:
      if (funct3 == 0x2) return FLW;
      if (funct3 == 0x3) return FLD;
      break;

    case 0x27:
      if (funct3 == 0x2) return FSW;
      if (funct3 == 0x3) return FSD;
      break;

    case 0x43:
      if ((funct7 & 0x3) == 0x0) return FMADD_S;
      if ((funct7 & 0x3) == 0x1) return FMADD_D;
      break;

    case 0x47:
      if ((funct7 & 0x3) == 0x0) return FMSUB_S;
      if ((funct7 & 0x3) == 0x1) return FMSUB_D;
      break;

    case 0x4B:
      if ((funct7 & 0x3) == 0x0) return FNMSUB_S;
      if ((funct7 & 0x3) == 0x1) return FNMSUB_D;
      break;

    case 0x4F:
      if ((funct7 & 0x3) == 0x0) return FNMADD_S;
      if ((funct7 & 0x3) == 0x1) return FNMADD_D;
      break;

    case 0x53:
      switch (funct7) {
        case 0x00: return FADD_S;
        case 0x04: return FSUB_S;
        case 0x08: return FMUL_S;
        case 0x0C: return FDIV_S;
        case 0x2C: return FSQRT_S;
        case 0x10:
          switch (funct3) {
            case 0x0: return FSGNJ_S;
            case 0x1: return FSGNJN_S;
            case 0x2: return FSGNJX_S;
          }
          break;
        case 0x14:
          if (funct3 == 0x0) return FMIN_S;
          if (funct3 == 0x1) return FMAX_S;
          break;
        case 0x60:
          if (funct3 == 0x0) return FCVT_W_S;
          if (funct3 == 0x1) return FCVT_WU_S;
          break;
        case 0x70:
          if (funct3 == 0x0) return FMV_X_W;
          if (funct3 == 0x1) return FCLASS_S;
          break;
        case 0x50:
          switch (funct3) {
            case 0x2: return FEQ_S;
            case 0x1: return FLT_S;
            case 0x0: return FLE_S;
          }
          break;
        case 0x68:
          if (funct3 == 0x0) return FCVT_S_W;
          if (funct3 == 0x1) return FCVT_S_WU;
          break;
        case 0x78: return FMV_W_X;

        case 0x01: return FADD_D;
        case 0x05: return FSUB_D;
        case 0x09: return FMUL_D;
        case 0x0D: return FDIV_D;
        case 0x2D: return FSQRT_D;
        case 0x11:
          switch (funct3) {
            case 0x0: return FSGNJ_D;
            case 0x1: return FSGNJN_D;
            case 0x2: return FSGNJX_D;
          }
          break;
        case 0x15:
          if (funct3 == 0x0) return FMIN_D;
          if (funct3 == 0x1) return FMAX_D;
          break;
        case 0x20: return FCVT_S_D;
        case 0x21: return FCVT_D_S;
        case 0x61:
          if (funct3 == 0x0) return FCVT_W_D;
          if (funct3 == 0x1) return FCVT_WU_D;
          break;
        case 0x51:
          switch (funct3) {
            case 0x2: return FEQ_D;
            case 0x1: return FLT_D;
            case 0x0: return FLE_D;
          }
          break;
        case 0x71: return FCLASS_D;
        case 0x69:
          if (funct3 == 0x0) return FCVT_D_W;
          if (funct3 == 0x1) return FCVT_D_WU;
          break;
      }
      break;
  }

  if ((opcode & 0x3) != 0x3) {
    u16 c_inst = opcode & 0xFFFF;
    u32 c_op = c_inst & 0x3;
    u32 c_funct3 = (c_inst >> 13) & 0x7;

    switch (c_op) {
      case 0x0:
        switch (c_funct3) {
          case 0x0: return C_ADDI4SPN;
          case 0x2: return C_LW;
          case 0x6: return C_SW;
          case 0x1: return C_FLD;
          case 0x5: return C_FSD;
          case 0x3: return C_FLW;
          case 0x7: return C_FSW;
        }
        break;

      case 0x1:
        switch (c_funct3) {
          case 0x0: return C_ADDI;
          case 0x1: return C_JAL;
          case 0x2: return C_LI;
          case 0x3:
            if (((c_inst >> 7) & 0x1F) == 2) return C_ADDI16SP;
            else return C_LUI;
          case 0x4: {
            u32 c_funct2 = (c_inst >> 10) & 0x3;
            switch (c_funct2) {
              case 0x0: return C_SRLI;
              case 0x1: return C_SRAI;
              case 0x2: return C_ANDI;
              case 0x3: {
                u32 c_funct6 = (c_inst >> 10) & 0x3F;
                u32 c_funct2_low = c_funct6 & 0x3;
                u32 c_bit12 = (c_funct6 >> 2) & 0x1;
                if (c_bit12 == 0) {
                  switch (c_funct2_low) {
                    case 0x0: return C_SUB;
                    case 0x1: return C_XOR;
                    case 0x2: return C_OR;
                    case 0x3: return C_AND;
                  }
                }
              }
              break;
            }
          }
          break;
          case 0x5: return C_J;
          case 0x6: return C_BEQZ;
          case 0x7: return C_BNEZ;
        }
        break;

      case 0x2:
        switch (c_funct3) {
          case 0x0: return C_SLLI;
          case 0x2: return C_LWSP;
          case 0x4: {
            u32 c_bit12 = (c_inst >> 12) & 0x1;
            u32 rs1 = (c_inst >> 7) & 0x1F;
            u32 rs2 = (c_inst >> 2) & 0x1F;

            if (c_bit12 == 0) {
              if (rs2 == 0) return C_JR;
              else return C_MV;
            } else {
              if (rs1 == 0 && rs2 == 0) return C_EBREAK;
              else if (rs2 == 0) return C_JALR;
              else return C_ADD;
            }
          }
          break;
          case 0x6: return C_SWSP;
          case 0x1: return C_FLDSP;
          case 0x5: return C_FSDSP;
          case 0x3: return C_FLWSP;
          case 0x7: return C_FSWSP;
        }
        break;
    }
    return INSTR_INVALID;
  }

  return INSTR_INVALID;
}

#define _rd rv->x[rd]
#define _rs1 rv->x[rs1]
#define _rs2 rv->x[rs2]
#define _frd rv->f[rd]
#define _frs1 rv->f[rs1]
#define _frs2 rv->f[rs2]
#define _frs3 rv->f[rs3]
#define _load8(addr) rv_mem_read8(rv, addr)
#define _load16(addr) rv_mem_read16(rv, addr)
#define _load32(addr) rv_mem_read32(rv, addr)
#define _load_f64(addr) rv_mem_read_f64(rv, addr)
#define _store8(addr, val) rv_mem_write8(rv, addr, val)
#define _store16(addr, val) rv_mem_write16(rv, addr, val)
#define _store32(addr, val) rv_mem_write32(rv, addr, val)
#define _store_f64(addr, val) rv_mem_write_f64(rv, addr, val)
#define _branch(cond) if (cond) rv->pc += imm_b - 4
#define _c_branch(cond, off) if (cond) rv->pc += off - 2
#define _nan_box() _frd.raw |= 0xFFFFFFFF00000000ULL
#define _c_rd get_c_rd_rs1(c_inst)
#define _c_rs1_p get_c_rs1_prime(c_inst)
#define _c_rs2_p get_c_rs2_prime(c_inst)
#define _c_rd_p get_c_rd_prime(c_inst)
#define _c_rs2 get_c_rs2(c_inst)

void rv_interpret(rv_t *rv, u32 opcode) {
  instr_e instr = rv_decode(opcode);
  u32 rd = get_rd(opcode);
  u32 rs1 = get_rs1(opcode);
  u32 rs2 = get_rs2(opcode);
  s32 imm_i = get_i_imm(opcode);
  s32 imm_s = get_s_imm(opcode);
  s32 imm_b = get_b_imm(opcode);
  s32 imm_u = get_u_imm(opcode);
  s32 imm_j = get_j_imm(opcode);
  u16 c_inst = opcode & 0xFFFF;

  switch (instr) {
    case ADD: _rd = _rs1 + _rs2; break;
    case ADDI: _rd = _rs1 + imm_i; break;
    case SUB: _rd = _rs1 - _rs2; break;
    case LUI: _rd = imm_u; break;
    case AUIPC: _rd = rv->pc + imm_u; break;

    case XOR: _rd = _rs1 ^ _rs2; break;
    case XORI: _rd = _rs1 ^ imm_i; break;
    case OR: _rd = _rs1 | _rs2; break;
    case ORI: _rd = _rs1 | imm_i; break;
    case AND: _rd = _rs1 & _rs2; break;
    case ANDI: _rd = _rs1 & imm_i; break;

    case SLL: _rd = _rs1 << (_rs2 & 0x1F); break;
    case SLLI: _rd = _rs1 << (imm_i & 0x1F); break;
    case SRL: _rd = _rs1 >> (_rs2 & 0x1F); break;
    case SRLI: _rd = _rs1 >> (imm_i & 0x1F); break;
    case SRA: _rd = (s32)_rs1 >> (_rs2 & 0x1F); break;
    case SRAI: _rd = (s32)_rs1 >> (imm_i & 0x1F); break;

    case SLT: _rd = ((s32)_rs1 < (s32)_rs2) ? 1 : 0; break;
    case SLTI: _rd = ((s32)_rs1 < imm_i) ? 1 : 0; break;
    case SLTU: _rd = (_rs1 < _rs2) ? 1 : 0; break;
    case SLTIU: _rd = (_rs1 < (u32)imm_i) ? 1 : 0; break;

    case BEQ: _branch(_rs1 == _rs2); break;
    case BNE: _branch(_rs1 != _rs2); break;
    case BLT: _branch((s32)_rs1 < (s32)_rs2); break;
    case BGE: _branch((s32)_rs1 >= (s32)_rs2); break;
    case BLTU: _branch(_rs1 < _rs2); break;
    case BGEU: _branch(_rs1 >= _rs2); break;

    case JAL: _rd = rv->pc + 4; rv->pc += imm_j - 4; break;
    case JALR: {
      u32 target = (_rs1 + imm_i) & ~1;
      _rd = rv->pc + 4;
      rv->pc = target - 4;
    } break;

    case LB: _rd = sign_extend(_load8(_rs1 + imm_i), 8); break;
    case LH: _rd = sign_extend(_load16(_rs1 + imm_i), 16); break;
    case LW: _rd = _load32(_rs1 + imm_i); break;
    case LBU: _rd = _load8(_rs1 + imm_i); break;
    case LHU: _rd = _load16(_rs1 + imm_i); break;

    case SB: _store8(_rs1 + imm_s, _rs2); break;
    case SH: _store16(_rs1 + imm_s, _rs2); break;
    case SW: _store32(_rs1 + imm_s, _rs2); break;

    case ECALL: case EBREAK: case FENCE: case FENCE_I: break;

    case CSRRW: case CSRRS: case CSRRC: case CSRRWI: case CSRRSI: case CSRRCI: {
      u32 csr = imm_i & 0xFFF;
      u32 old_val = rv_csr_read(rv, csr);
      _rd = old_val;
      switch (instr) {
        case CSRRW: rv_csr_write(rv, csr, _rs1); break;
        case CSRRS: if (rs1) rv_csr_write(rv, csr, old_val | _rs1); break;
        case CSRRC: if (rs1) rv_csr_write(rv, csr, old_val & ~_rs1); break;
        case CSRRWI: rv_csr_write(rv, csr, rs1); break;
        case CSRRSI: if (rs1) rv_csr_write(rv, csr, old_val | rs1); break;
        case CSRRCI: if (rs1) rv_csr_write(rv, csr, old_val & ~rs1); break;
      }
    } break;

    case MUL: _rd = _rs1 * _rs2; break;
    case MULH: _rd = ((s64)(s32)_rs1 * (s64)(s32)_rs2) >> 32; break;
    case MULHSU: _rd = ((s64)(s32)_rs1 * (u64)_rs2) >> 32; break;
    case MULHU: _rd = ((u64)_rs1 * (u64)_rs2) >> 32; break;

    case DIV:
      if (_rs2 == 0) _rd = -1;
      else if (_rs1 == 0x80000000 && _rs2 == 0xFFFFFFFF) _rd = 0x80000000;
      else _rd = (s32)_rs1 / (s32)_rs2;
      break;

    case DIVU: _rd = _rs2 ? _rs1 / _rs2 : 0xFFFFFFFF; break;

    case REM:
      if (_rs2 == 0) _rd = _rs1;
      else if (_rs1 == 0x80000000 && _rs2 == 0xFFFFFFFF) _rd = 0;
      else _rd = (s32)_rs1 % (s32)_rs2;
      break;

    case REMU: _rd = _rs2 ? _rs1 % _rs2 : _rs1; break;

    case C_ADDI: {
      u32 c_rd = _c_rd;
      s32 c_imm = sign_extend(((c_inst >> 12) & 1) << 5 | ((c_inst >> 2) & 0x1F), 6);
      if (c_rd) rv->x[c_rd] += c_imm;
    } break;

    case C_LI: {
      u32 c_rd = _c_rd;
      s32 c_imm = sign_extend(((c_inst >> 12) & 1) << 5 | ((c_inst >> 2) & 0x1F), 6);
      rv->x[c_rd] = c_imm;
    } break;

    case C_LW: {
      u32 c_rd = _c_rd_p, c_rs1 = _c_rs1_p;
      u32 c_offset = ((c_inst >> 6) & 1) << 2 | ((c_inst >> 10) & 0x7) << 3 | ((c_inst >> 5) & 1) << 6;
      rv->x[c_rd] = _load32(rv->x[c_rs1] + c_offset);
    } break;

    case C_SW: {
      u32 c_rs1 = _c_rs1_p, c_rs2 = _c_rs2_p;
      u32 c_offset = ((c_inst >> 6) & 1) << 2 | ((c_inst >> 10) & 0x7) << 3 | ((c_inst >> 5) & 1) << 6;
      _store32(rv->x[c_rs1] + c_offset, rv->x[c_rs2]);
    } break;

    case C_J: {
      s32 c_offset = sign_extend(((c_inst >> 12) & 1) << 11 | ((c_inst >> 8) & 1) << 4 |
                                 ((c_inst >> 9) & 0x3) << 8 | ((c_inst >> 6) & 1) << 7 |
                                 ((c_inst >> 7) & 1) << 6 | ((c_inst >> 2) & 1) << 5 |
                                 ((c_inst >> 11) & 1) << 1 | ((c_inst >> 3) & 0x7) << 2, 12);
      rv->pc += c_offset - 2;
    } break;

    case C_JAL: {
      s32 c_offset = sign_extend(((c_inst >> 12) & 1) << 11 | ((c_inst >> 8) & 1) << 4 |
                                 ((c_inst >> 9) & 0x3) << 8 | ((c_inst >> 6) & 1) << 7 |
                                 ((c_inst >> 7) & 1) << 6 | ((c_inst >> 2) & 1) << 5 |
                                 ((c_inst >> 11) & 1) << 1 | ((c_inst >> 3) & 0x7) << 2, 12);
      rv->x[1] = rv->pc + 2;
      rv->pc += c_offset - 2;
    } break;

    case C_JR: rv->pc = rv->x[_c_rd] - 2; break;
    case C_JALR: {
      u32 target = rv->x[_c_rd];
      rv->x[1] = rv->pc + 2;
      rv->pc = target - 2;
    } break;

    case C_BEQZ: {
      s32 c_offset = sign_extend(((c_inst >> 12) & 1) << 8 | ((c_inst >> 6) & 0x3) << 3 |
                                 ((c_inst >> 5) & 1) << 7 | ((c_inst >> 2) & 1) << 6 |
                                 ((c_inst >> 10) & 0x3) << 1 | ((c_inst >> 3) & 0x3) << 5, 9);
      _c_branch(rv->x[_c_rs1_p] == 0, c_offset);
    } break;

    case C_BNEZ: {
      s32 c_offset = sign_extend(((c_inst >> 12) & 1) << 8 | ((c_inst >> 6) & 0x3) << 3 |
                                 ((c_inst >> 5) & 1) << 7 | ((c_inst >> 2) & 1) << 6 |
                                 ((c_inst >> 10) & 0x3) << 1 | ((c_inst >> 3) & 0x3) << 5, 9);
      _c_branch(rv->x[_c_rs1_p] != 0, c_offset);
    } break;

    case C_LUI: {
      s32 c_imm = sign_extend(((c_inst >> 12) & 1) << 17 | ((c_inst >> 2) & 0x1F) << 12, 18);
      rv->x[_c_rd] = c_imm;
    } break;

    case C_ADDI16SP: {
      s32 c_imm = sign_extend(((c_inst >> 12) & 1) << 9 | ((c_inst >> 4) & 0x3) << 7 |
                              ((c_inst >> 3) & 1) << 6 | ((c_inst >> 5) & 1) << 4 |
                              ((c_inst >> 2) & 1) << 5, 10);
      rv->x[2] += c_imm;
    } break;

    case C_ADDI4SPN: {
      u32 c_imm = ((c_inst >> 11) & 0x3) << 4 | ((c_inst >> 7) & 0xF) << 6 |
                  ((c_inst >> 6) & 1) << 2 | ((c_inst >> 5) & 1) << 3;
      rv->x[_c_rd_p] = rv->x[2] + c_imm;
    } break;

    case C_SLLI: rv->x[_c_rd] <<= ((c_inst >> 12) & 1) << 5 | ((c_inst >> 2) & 0x1F); break;
    case C_SRLI: rv->x[_c_rs1_p] >>= ((c_inst >> 12) & 1) << 5 | ((c_inst >> 2) & 0x1F); break;
    case C_SRAI: rv->x[_c_rs1_p] = (s32)rv->x[_c_rs1_p] >> (((c_inst >> 12) & 1) << 5 | ((c_inst >> 2) & 0x1F)); break;

    case C_ANDI: {
      s32 c_imm = sign_extend(((c_inst >> 12) & 1) << 5 | ((c_inst >> 2) & 0x1F), 6);
      rv->x[_c_rs1_p] &= c_imm;
    } break;

    case C_MV: rv->x[_c_rd] = rv->x[_c_rs2]; break;
    case C_ADD: rv->x[_c_rd] += rv->x[_c_rs2]; break;
    case C_SUB: rv->x[_c_rs1_p] -= rv->x[_c_rs2_p]; break;
    case C_XOR: rv->x[_c_rs1_p] ^= rv->x[_c_rs2_p]; break;
    case C_OR: rv->x[_c_rs1_p] |= rv->x[_c_rs2_p]; break;
    case C_AND: rv->x[_c_rs1_p] &= rv->x[_c_rs2_p]; break;

    case C_LWSP: {
      u32 c_offset = ((c_inst >> 4) & 0x7) << 2 | ((c_inst >> 12) & 1) << 5 | ((c_inst >> 2) & 0x3) << 6;
      rv->x[_c_rd] = _load32(rv->x[2] + c_offset);
    } break;

    case C_SWSP: {
      u32 c_offset = ((c_inst >> 9) & 0xF) << 2 | ((c_inst >> 7) & 0x3) << 6;
      _store32(rv->x[2] + c_offset, rv->x[_c_rs2]);
    } break;

    case C_EBREAK: break;

    case C_FLW: case C_FSW: case C_FLWSP: case C_FSWSP: case C_FLD: case C_FSD: case C_FLDSP: case C_FSDSP: break;

    case FLW: _frd.raw = 0xFFFFFFFF00000000ULL | _load32(_rs1 + imm_i); break;
    case FSW: _store32(_rs1 + imm_s, _frs2.raw & 0xFFFFFFFF); break;

    case FADD_S: _frd.f32 = _frs1.f32 + _frs2.f32; _nan_box(); break;
    case FSUB_S: _frd.f32 = _frs1.f32 - _frs2.f32; _nan_box(); break;
    case FMUL_S: _frd.f32 = _frs1.f32 * _frs2.f32; _nan_box(); break;
    case FDIV_S: _frd.f32 = _frs1.f32 / _frs2.f32; _nan_box(); break;
    case FSQRT_S: _frd.f32 = sqrtf(_frs1.f32); _nan_box(); break;

    case FSGNJ_S: {
      u32 src1 = _frs1.raw & 0xFFFFFFFF, src2 = _frs2.raw & 0xFFFFFFFF;
      _frd.raw = 0xFFFFFFFF00000000ULL | (src1 & 0x7FFFFFFF) | (src2 & 0x80000000);
    } break;

    case FSGNJN_S: {
      u32 src1 = _frs1.raw & 0xFFFFFFFF, src2 = _frs2.raw & 0xFFFFFFFF;
      _frd.raw = 0xFFFFFFFF00000000ULL | (src1 & 0x7FFFFFFF) | ((~src2) & 0x80000000);
    } break;

    case FSGNJX_S: {
      u32 src1 = _frs1.raw & 0xFFFFFFFF, src2 = _frs2.raw & 0xFFFFFFFF;
      _frd.raw = 0xFFFFFFFF00000000ULL | src1 ^ (src2 & 0x80000000);
    } break;

    case FMIN_S: _frd.f32 = (_frs1.f32 < _frs2.f32) ? _frs1.f32 : _frs2.f32; _nan_box(); break;
    case FMAX_S: _frd.f32 = (_frs1.f32 > _frs2.f32) ? _frs1.f32 : _frs2.f32; _nan_box(); break;

    case FCVT_W_S: _rd = (s32)_frs1.f32; break;
    case FCVT_WU_S: _rd = (u32)_frs1.f32; break;
    case FCVT_S_W: _frd.f32 = (f32)(s32)_rs1; _nan_box(); break;
    case FCVT_S_WU: _frd.f32 = (f32)_rs1; _nan_box(); break;

    case FMV_X_W: _rd = _frs1.raw & 0xFFFFFFFF; break;
    case FMV_W_X: _frd.raw = 0xFFFFFFFF00000000ULL | _rs1; break;

    case FEQ_S: _rd = (_frs1.f32 == _frs2.f32) ? 1 : 0; break;
    case FLT_S: _rd = (_frs1.f32 < _frs2.f32) ? 1 : 0; break;
    case FLE_S: _rd = (_frs1.f32 <= _frs2.f32) ? 1 : 0; break;
    case FCLASS_S: _rd = rv_fclass_s(_frs1.f32); break;

    case FMADD_S: case FMSUB_S: case FNMSUB_S: case FNMADD_S: {
      u32 rs3 = (opcode >> 27) & 0x1F;
      f32 prod = _frs1.f32 * _frs2.f32;
      switch (instr) {
        case FMADD_S: _frd.f32 = prod + _frs3.f32; break;
        case FMSUB_S: _frd.f32 = prod - _frs3.f32; break;
        case FNMSUB_S: _frd.f32 = -prod + _frs3.f32; break;
        case FNMADD_S: _frd.f32 = -prod - _frs3.f32; break;
      }
      _nan_box();
    } break;

    case FLD: _frd.f64 = _load_f64(_rs1 + imm_i); break;
    case FSD: _store_f64(_rs1 + imm_s, _frs2.f64); break;

    case FADD_D: _frd.f64 = _frs1.f64 + _frs2.f64; break;
    case FSUB_D: _frd.f64 = _frs1.f64 - _frs2.f64; break;
    case FMUL_D: _frd.f64 = _frs1.f64 * _frs2.f64; break;
    case FDIV_D: _frd.f64 = _frs1.f64 / _frs2.f64; break;
    case FSQRT_D: _frd.f64 = sqrt(_frs1.f64); break;

    case FSGNJ_D: {
      u64 src1 = _frs1.raw, src2 = _frs2.raw;
      _frd.raw = (src1 & 0x7FFFFFFFFFFFFFFFULL) | (src2 & 0x8000000000000000ULL);
    } break;

    case FSGNJN_D: {
      u64 src1 = _frs1.raw, src2 = _frs2.raw;
      _frd.raw = (src1 & 0x7FFFFFFFFFFFFFFFULL) | ((~src2) & 0x8000000000000000ULL);
    } break;

    case FSGNJX_D: {
      u64 src1 = _frs1.raw, src2 = _frs2.raw;
      _frd.raw = src1 ^ (src2 & 0x8000000000000000ULL);
    } break;

    case FMIN_D: _frd.f64 = (_frs1.f64 < _frs2.f64) ? _frs1.f64 : _frs2.f64; break;
    case FMAX_D: _frd.f64 = (_frs1.f64 > _frs2.f64) ? _frs1.f64 : _frs2.f64; break;

    case FCVT_S_D: _frd.f32 = (f32)_frs1.f64; _nan_box(); break;
    case FCVT_D_S: _frd.f64 = (f64)_frs1.f32; break;

    case FCVT_W_D: _rd = (s32)_frs1.f64; break;
    case FCVT_WU_D: _rd = (u32)_frs1.f64; break;
    case FCVT_D_W: _frd.f64 = (f64)(s32)_rs1; break;
    case FCVT_D_WU: _frd.f64 = (f64)_rs1; break;

    case FEQ_D: _rd = (_frs1.f64 == _frs2.f64) ? 1 : 0; break;
    case FLT_D: _rd = (_frs1.f64 < _frs2.f64) ? 1 : 0; break;
    case FLE_D: _rd = (_frs1.f64 <= _frs2.f64) ? 1 : 0; break;
    case FCLASS_D: _rd = rv_fclass_d(_frs1.f64); break;

    case FMADD_D: case FMSUB_D: case FNMSUB_D: case FNMADD_D: {
      u32 rs3 = (opcode >> 27) & 0x1F;
      f64 prod = _frs1.f64 * _frs2.f64;
      switch (instr) {
        case FMADD_D: _frd.f64 = prod + _frs3.f64; break;
        case FMSUB_D: _frd.f64 = prod - _frs3.f64; break;
        case FNMSUB_D: _frd.f64 = -prod + _frs3.f64; break;
        case FNMADD_D: _frd.f64 = -prod - _frs3.f64; break;
      }
    } break;

    case LR_W: _rd = _load32(_rs1); rv_set_reservation(rv, _rs1); break;
    case SC_W:
      if (rv_check_reservation(rv, _rs1)) {
        _store32(_rs1, _rs2);
        _rd = 0;
        rv_clear_reservation(rv);
      } else _rd = 1;
      break;

    case AMOSWAP_W: case AMOADD_W: case AMOXOR_W: case AMOAND_W: case AMOOR_W:
    case AMOMIN_W: case AMOMAX_W: case AMOMINU_W: case AMOMAXU_W: {
      u32 addr = _rs1, old_val = _load32(addr), new_val;
      switch (instr) {
        case AMOSWAP_W: new_val = _rs2; break;
        case AMOADD_W: new_val = old_val + _rs2; break;
        case AMOXOR_W: new_val = old_val ^ _rs2; break;
        case AMOAND_W: new_val = old_val & _rs2; break;
        case AMOOR_W: new_val = old_val | _rs2; break;
        case AMOMIN_W: new_val = ((s32)_rs2 < (s32)old_val) ? _rs2 : old_val; break;
        case AMOMAX_W: new_val = ((s32)_rs2 > (s32)old_val) ? _rs2 : old_val; break;
        case AMOMINU_W: new_val = (_rs2 < old_val) ? _rs2 : old_val; break;
        case AMOMAXU_W: new_val = (_rs2 > old_val) ? _rs2 : old_val; break;
      }
      _store32(addr, new_val);
      _rd = old_val;
    } break;

    case INSTR_INVALID: default: break;
  }

  rv->x[0] = 0;

  if (instr != JAL && instr != JALR &&
      instr != BEQ && instr != BNE && instr != BLT && instr != BGE && instr != BLTU && instr != BGEU &&
      instr != C_J && instr != C_JAL && instr != C_JR && instr != C_JALR &&
      instr != C_BEQZ && instr != C_BNEZ) {
    rv->pc += ((opcode & 0x3) != 0x3) ? 2 : 4;
  }
}

#undef _rd
#undef _rs1
#undef _rs2
#undef _frd
#undef _frs1
#undef _frs2
#undef _frs3
#undef _load8
#undef _load16
#undef _load32
#undef _load_f64
#undef _store8
#undef _store16
#undef _store32
#undef _store_f64
#undef _branch
#undef _c_branch
#undef _nan_box
#undef _c_rd
#undef _c_rs1_p
#undef _c_rs2_p
#undef _c_rd_p
#undef _c_rs2

// Complete emulator step function
int rv_step(rv_t *rv) {
  // Update timers and counters
  rv_update_timer(rv);

  // Check for interrupts
  rv_check_interrupts(rv);

  // Fetch instruction
  u32 opcode = rv_fetch_instruction(rv, rv->pc);

  // Execute instruction
  rv_interpret(rv, opcode);

  return 0; // Success
}

// User mode support functions
void rv_setup_user_mode(rv_t *rv, u32 entry_point, u32 stack_top) {
  // Initialize all registers to 0
  memset(rv->x, 0, sizeof(rv->x));
  memset(rv->f, 0, sizeof(rv->f));

  // Set up user mode execution
  rv->pc = entry_point;
  rv->sp = stack_top;

  // Set up basic user environment
  rv->gp = 0x10000; // Global pointer
  rv->tp = 0x20000; // Thread pointer

  // Start in user mode
  rv->csr.priv = PRIV_USER;

  // Set up basic status
  rv->csr.mstatus = 0;
  rv->csr.sstatus = 0;

  // Clear any pending exceptions
  rv->csr.mcause = 0;
  rv->csr.scause = 0;

  printf("User mode setup: PC=0x%08x, SP=0x%08x, priv=%u\n", rv->pc, rv->sp, rv->csr.priv);
}

int rv_load_user_elf(rv_t *rv, const char *filename) {
  u32 entry_point, stack_top;

  // Load ELF file
  if (elf_load(rv->ram, MEMORY_SIZE, filename, &entry_point, &stack_top) != 0) {
    return -1;
  }

  // Set up user mode execution
  rv_setup_user_mode(rv, entry_point, stack_top);

  return 0;
}
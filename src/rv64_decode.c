#include "common.h"
#include "rv64gc.h"
#include "rv64_opcodes.h"

// Bitfield extraction macros
#define BITS(x, hi, lo) (((x) >> (lo)) & ((1ULL << ((hi) - (lo) + 1)) - 1))
#define BIT(x, n) BITS(x, n, n)
#define SEXT(x, bits) ((s64)(((u64)(x) << (64 - (bits))) >> (64 - (bits))))

// Instruction field extraction
#define GET_OPCODE(x) BITS(x, 6, 0)
#define GET_RD(x) BITS(x, 11, 7)
#define GET_FUNCT3(x) BITS(x, 14, 12)
#define GET_RS1(x) BITS(x, 19, 15)
#define GET_RS2(x) BITS(x, 24, 20)
#define GET_RS3(x) BITS(x, 31, 27)
#define GET_FUNCT7(x) BITS(x, 31, 25)
#define GET_FUNCT6(x) BITS(x, 31, 26)

// Immediate extraction macros
#define I_IMM(x) SEXT(BITS(x, 31, 20), 12)
#define S_IMM(x) SEXT((BITS(x, 31, 25) << 5) | BITS(x, 11, 7), 12)
#define B_IMM(x) SEXT((BIT(x, 31) << 12) | (BIT(x, 7) << 11) | (BITS(x, 30, 25) << 5) | (BITS(x, 11, 8) << 1), 13)
#define U_IMM(x) SEXT(x & 0xfffff000, 32)
#define J_IMM(x) SEXT((BIT(x, 31) << 20) | (BITS(x, 19, 12) << 12) | (BIT(x, 20) << 11) | (BITS(x, 30, 21) << 1), 21)

// Compressed instruction fields
#define C_OP(x) BITS(x, 1, 0)
#define C_FUNCT3(x) BITS(x, 15, 13)
#define C_FUNCT4(x) BITS(x, 15, 12)
#define C_RD_RS1(x) BITS(x, 11, 7)
#define C_RS2(x) BITS(x, 6, 2)
#define C_RD_RS1_P(x) (BITS(x, 9, 7) + 8)
#define C_RS2_P(x) (BITS(x, 4, 2) + 8)

// Opcode definitions
enum {
  OP_LUI = 0x37, OP_AUIPC = 0x17, OP_JAL = 0x6f, OP_JALR = 0x67,
  OP_BRANCH = 0x63, OP_LOAD = 0x03, OP_STORE = 0x23, OP_IMM = 0x13,
  OP_IMM_32 = 0x1b, OP_REG = 0x33, OP_REG_32 = 0x3b, OP_FENCE = 0x0f,
  OP_SYSTEM = 0x73, OP_AMO = 0x2f, OP_FP_LOAD = 0x07, OP_FP_STORE = 0x27,
  OP_FP_MADD = 0x43, OP_FP_MSUB = 0x47, OP_FP_NMSUB = 0x4b, OP_FP_NMADD = 0x4f,
  OP_FP = 0x53
};

// Macro to set opcode based on funct3 with an array lookup
#define SET_OP_BY_FUNCT3(base, ...) do { \
  static const rv_opcode_t ops[] = { __VA_ARGS__ }; \
  dec.opcode = (funct3 < sizeof(ops)/sizeof(ops[0])) ? ops[funct3] : RV_OP_INVALID; \
} while(0)

// Macro for shift instructions with funct6/funct7 checks
#define DECODE_SHIFT(op_sll, op_srl, op_sra, shamt_bits) do { \
  if (funct3 == 1 && funct6 == 0) { \
    dec.opcode = op_sll; \
    dec.imm = BITS(inst, shamt_bits, 20); \
  } else if (funct3 == 5) { \
    if (funct6 == 0) { \
      dec.opcode = op_srl; \
      dec.imm = BITS(inst, shamt_bits, 20); \
    } else if (funct6 == 0x10) { \
      dec.opcode = op_sra; \
      dec.imm = BITS(inst, shamt_bits, 20); \
    } else { \
      dec.opcode = RV_OP_INVALID; \
    } \
  } \
} while(0)

// Macro for R-type instructions
#define DECODE_R_TYPE(f7_base, ...) do { \
  if (funct7 == f7_base) { \
    static const rv_opcode_t ops[] = { __VA_ARGS__ }; \
    dec.opcode = (funct3 < sizeof(ops)/sizeof(ops[0])) ? ops[funct3] : RV_OP_INVALID; \
  } \
} while(0)

// Compressed instruction decoding helpers
#define C_IMM_ADDI4SPN(inst) ((BITS(inst, 10, 7) << 6) | (BITS(inst, 12, 11) << 4) | \
               (BIT(inst, 5) << 3) | (BIT(inst, 6) << 2))
#define C_IMM_LW(inst) ((BIT(inst, 5) << 2) | (BITS(inst, 12, 10) << 3) | (BIT(inst, 6) << 6))
#define C_IMM_LD(inst) ((BITS(inst, 12, 10) << 3) | (BIT(inst, 6) << 6) | (BIT(inst, 5) << 2))

static rv_inst_t decode_compressed(u16 inst) {
  rv_inst_t dec = {0};
  dec.raw = inst;
  
  u32 op = C_OP(inst);
  u32 funct3 = C_FUNCT3(inst);
  
  if (op == 0) { // Quadrant 0
    switch (funct3) {
      case 0: // C.ADDI4SPN
        dec.opcode = RV_OP_C_ADDI4SPN;
        dec.rd = C_RD_RS1_P(inst);
        dec.rs1 = 2; // SP
        dec.imm = C_IMM_ADDI4SPN(inst);
        if (dec.imm == 0) dec.opcode = RV_OP_INVALID;
        break;
      case 1: // C.FLD
        dec.opcode = RV_OP_C_FLD;
        dec.rd = C_RD_RS1_P(inst);
        dec.rs1 = C_RD_RS1_P(inst);
        dec.imm = C_IMM_LD(inst);
        break;
      case 2: // C.LW
        dec.opcode = RV_OP_C_LW;
        dec.rd = C_RS2_P(inst);
        dec.rs1 = C_RD_RS1_P(inst);
        dec.imm = C_IMM_LW(inst);
        break;
      case 3: // C.FLW / C.LD
        dec.opcode = (1 /* RV64 */) ? RV_OP_C_LD : RV_OP_C_FLW;
        dec.rd = C_RS2_P(inst);
        dec.rs1 = C_RD_RS1_P(inst);
        dec.imm = (1 /* RV64 */) ? C_IMM_LD(inst) : C_IMM_LW(inst);
        break;
      case 4: // C.RESERVED
        dec.opcode = RV_OP_INVALID;
        break;
      case 5: // C.FSD
        dec.opcode = RV_OP_C_FSD;
        dec.rs2 = C_RS2_P(inst);
        dec.rs1 = C_RD_RS1_P(inst);
        dec.imm = C_IMM_LD(inst);
        break;
      case 6: // C.SW
        dec.opcode = RV_OP_C_SW;
        dec.rs2 = C_RS2_P(inst);
        dec.rs1 = C_RD_RS1_P(inst);
        dec.imm = C_IMM_LW(inst);
        break;
      case 7: // C.FSW / C.SD
        dec.opcode = (1 /* RV64 */) ? RV_OP_C_SD : RV_OP_C_FSW;
        dec.rs2 = C_RS2_P(inst);
        dec.rs1 = C_RD_RS1_P(inst);
        dec.imm = (1 /* RV64 */) ? C_IMM_LD(inst) : C_IMM_LW(inst);
        break;
    }
  } else if (op == 1) { // Quadrant 1
    switch (funct3) {
      case 0: // C.NOP / C.ADDI
        dec.opcode = RV_OP_C_ADDI;
        dec.rd = dec.rs1 = C_RD_RS1(inst);
        dec.imm = SEXT((BIT(inst, 12) << 5) | BITS(inst, 6, 2), 6);
        if (dec.rd == 0 && dec.imm == 0) dec.opcode = RV_OP_C_NOP;
        break;
      case 1: // C.JAL (RV32) / C.ADDIW (RV64)
        if (1 /* RV64 */) {
          dec.opcode = RV_OP_C_ADDIW;
          dec.rd = dec.rs1 = C_RD_RS1(inst);
          dec.imm = SEXT((BIT(inst, 12) << 5) | BITS(inst, 6, 2), 6);
        } else {
          dec.opcode = RV_OP_C_JAL;
          dec.rd = 1; // RA
          dec.imm = SEXT((BIT(inst, 12) << 11) | (BIT(inst, 11) << 4) |
                 (BITS(inst, 10, 9) << 8) | (BIT(inst, 8) << 10) |
                 (BIT(inst, 7) << 6) | (BIT(inst, 6) << 7) |
                 (BITS(inst, 5, 3) << 1) | (BIT(inst, 2) << 5), 12);
        }
        break;
      case 2: // C.LI
        dec.opcode = RV_OP_C_LI;
        dec.rd = C_RD_RS1(inst);
        dec.imm = SEXT((BIT(inst, 12) << 5) | BITS(inst, 6, 2), 6);
        break;
      case 3: // C.ADDI16SP / C.LUI
        if (C_RD_RS1(inst) == 2) {
          dec.opcode = RV_OP_C_ADDI16SP;
          dec.rd = dec.rs1 = 2;
          dec.imm = SEXT((BIT(inst, 12) << 9) | (BITS(inst, 6, 2) << 4) |
                 (BIT(inst, 5) << 6) | (BITS(inst, 4, 3) << 7), 10);
        } else {
          dec.opcode = RV_OP_C_LUI;
          dec.rd = C_RD_RS1(inst);
          dec.imm = SEXT((BIT(inst, 12) << 17) | (BITS(inst, 6, 2) << 12), 18);
        }
        if (dec.imm == 0) dec.opcode = RV_OP_INVALID;
        break;
      case 4: // C.SRLI / C.SRAI / C.ANDI / ALU ops
        {
          u32 funct2 = BITS(inst, 11, 10);
          dec.rd = dec.rs1 = C_RD_RS1_P(inst);
          if (funct2 == 0) { // C.SRLI
            dec.opcode = RV_OP_C_SRLI;
            dec.imm = (BIT(inst, 12) << 5) | BITS(inst, 6, 2);
          } else if (funct2 == 1) { // C.SRAI
            dec.opcode = RV_OP_C_SRAI;
            dec.imm = (BIT(inst, 12) << 5) | BITS(inst, 6, 2);
          } else if (funct2 == 2) { // C.ANDI
            dec.opcode = RV_OP_C_ANDI;
            dec.imm = SEXT((BIT(inst, 12) << 5) | BITS(inst, 6, 2), 6);
          } else { // ALU ops
            dec.rs2 = C_RS2_P(inst);
            u32 funct6 = BITS(inst, 12, 10);
            u32 funct2_sub = BITS(inst, 6, 5);
            if (funct6 == 0x23) {
              static const rv_opcode_t alu_ops[] = {
                RV_OP_C_SUB, RV_OP_C_XOR, RV_OP_C_OR, RV_OP_C_AND
              };
              dec.opcode = alu_ops[funct2_sub];
            } else if (funct6 == 0x27 && 1 /* RV64 */) {
              dec.opcode = (funct2_sub == 0) ? RV_OP_C_SUBW : RV_OP_C_ADDW;
            } else {
              dec.opcode = RV_OP_INVALID;
            }
          }
        }
        break;
      case 5: // C.J
        dec.opcode = RV_OP_C_J;
        dec.imm = SEXT((BIT(inst, 12) << 11) | (BIT(inst, 11) << 4) |
               (BITS(inst, 10, 9) << 8) | (BIT(inst, 8) << 10) |
               (BIT(inst, 7) << 6) | (BIT(inst, 6) << 7) |
               (BITS(inst, 5, 3) << 1) | (BIT(inst, 2) << 5), 12);
        break;
      case 6: // C.BEQZ
        dec.opcode = RV_OP_C_BEQZ;
        dec.rs1 = C_RD_RS1_P(inst);
        dec.imm = SEXT((BIT(inst, 12) << 8) | (BITS(inst, 11, 10) << 3) |
               (BITS(inst, 6, 5) << 6) | (BITS(inst, 4, 3) << 1) |
               (BIT(inst, 2) << 5), 9);
        break;
      case 7: // C.BNEZ
        dec.opcode = RV_OP_C_BNEZ;
        dec.rs1 = C_RD_RS1_P(inst);
        dec.imm = SEXT((BIT(inst, 12) << 8) | (BITS(inst, 11, 10) << 3) |
               (BITS(inst, 6, 5) << 6) | (BITS(inst, 4, 3) << 1) |
               (BIT(inst, 2) << 5), 9);
        break;
    }
  } else if (op == 2) { // Quadrant 2
    switch (funct3) {
      case 0: // C.SLLI
        dec.opcode = RV_OP_C_SLLI;
        dec.rd = dec.rs1 = C_RD_RS1(inst);
        dec.imm = (BIT(inst, 12) << 5) | BITS(inst, 6, 2);
        if (dec.imm == 0) dec.imm = 64; // RV64 special case
        break;
      case 1: // C.FLDSP
        dec.opcode = RV_OP_C_FLDSP;
        dec.rd = C_RD_RS1(inst);
        dec.rs1 = 2; // SP
        dec.imm = (BIT(inst, 12) << 5) | (BITS(inst, 4, 2) << 6) | (BITS(inst, 6, 5) << 3);
        break;
      case 2: // C.LWSP
        dec.opcode = RV_OP_C_LWSP;
        dec.rd = C_RD_RS1(inst);
        dec.rs1 = 2; // SP
        dec.imm = (BIT(inst, 12) << 5) | (BITS(inst, 6, 4) << 2) | (BITS(inst, 3, 2) << 6);
        break;
      case 3: // C.FLWSP / C.LDSP
        dec.opcode = (1 /* RV64 */) ? RV_OP_C_LDSP : RV_OP_C_FLWSP;
        dec.rd = C_RD_RS1(inst);
        dec.rs1 = 2; // SP
        dec.imm = (1 /* RV64 */) ? 
          ((BIT(inst, 12) << 5) | (BITS(inst, 4, 2) << 6) | (BITS(inst, 6, 5) << 3)) :
          ((BIT(inst, 12) << 5) | (BITS(inst, 6, 4) << 2) | (BITS(inst, 3, 2) << 6));
        break;
      case 4: // C.JR / C.MV / C.EBREAK / C.JALR / C.ADD
        if (BIT(inst, 12) == 0) {
          if (C_RS2(inst) == 0) { // C.JR
            dec.opcode = RV_OP_C_JR;
            dec.rs1 = C_RD_RS1(inst);
          } else { // C.MV
            dec.opcode = RV_OP_C_MV;
            dec.rd = C_RD_RS1(inst);
            dec.rs2 = C_RS2(inst);
          }
        } else {
          if (C_RD_RS1(inst) == 0 && C_RS2(inst) == 0) { // C.EBREAK
            dec.opcode = RV_OP_C_EBREAK;
          } else if (C_RS2(inst) == 0) { // C.JALR
            dec.opcode = RV_OP_C_JALR;
            dec.rs1 = C_RD_RS1(inst);
            dec.rd = 1; // RA
          } else { // C.ADD
            dec.opcode = RV_OP_C_ADD;
            dec.rd = dec.rs1 = C_RD_RS1(inst);
            dec.rs2 = C_RS2(inst);
          }
        }
        break;
      case 5: // C.FSDSP
        dec.opcode = RV_OP_C_FSDSP;
        dec.rs2 = C_RS2(inst);
        dec.rs1 = 2; // SP
        dec.imm = (BITS(inst, 12, 10) << 3) | (BITS(inst, 9, 7) << 6);
        break;
      case 6: // C.SWSP
        dec.opcode = RV_OP_C_SWSP;
        dec.rs2 = C_RS2(inst);
        dec.rs1 = 2; // SP
        dec.imm = (BITS(inst, 12, 9) << 2) | (BITS(inst, 8, 7) << 6);
        break;
      case 7: // C.FSWSP / C.SDSP
        dec.opcode = (1 /* RV64 */) ? RV_OP_C_SDSP : RV_OP_C_FSWSP;
        dec.rs2 = C_RS2(inst);
        dec.rs1 = 2; // SP
        dec.imm = (1 /* RV64 */) ?
          ((BITS(inst, 12, 10) << 3) | (BITS(inst, 9, 7) << 6)) :
          ((BITS(inst, 12, 9) << 2) | (BITS(inst, 8, 7) << 6));
        break;
    }
  } else {
    dec.opcode = RV_OP_INVALID;
  }
  
  return dec;
}

rv_inst_t rv64_decode(u32 inst) {
  rv_inst_t dec = {0};
  dec.raw = inst;
  
  // Check if compressed instruction
  if ((inst & 0x3) != 0x3) {
    return decode_compressed(inst & 0xFFFF);
  }
  
  u32 opcode = GET_OPCODE(inst);
  u32 funct3 = GET_FUNCT3(inst);
  u32 funct7 = GET_FUNCT7(inst);
  u32 funct6 = GET_FUNCT6(inst);
  
  dec.rd = GET_RD(inst);
  dec.rs1 = GET_RS1(inst);
  dec.rs2 = GET_RS2(inst);
  dec.rs3 = GET_RS3(inst);
  
  switch (opcode) {
    case OP_LUI:
      dec.opcode = RV_OP_LUI;
      dec.imm = U_IMM(inst);
      break;
      
    case OP_AUIPC:
      dec.opcode = RV_OP_AUIPC;
      dec.imm = U_IMM(inst);
      break;
      
    case OP_JAL:
      dec.opcode = RV_OP_JAL;
      dec.imm = J_IMM(inst);
      break;
      
    case OP_JALR:
      dec.opcode = RV_OP_JALR;
      dec.imm = I_IMM(inst);
      break;
      
    case OP_BRANCH:
      dec.imm = B_IMM(inst);
      SET_OP_BY_FUNCT3(0, RV_OP_BEQ, RV_OP_BNE, RV_OP_INVALID, RV_OP_INVALID,
                RV_OP_BLT, RV_OP_BGE, RV_OP_BLTU, RV_OP_BGEU);
      break;
      
    case OP_LOAD:
      dec.imm = I_IMM(inst);
      SET_OP_BY_FUNCT3(0, RV_OP_LB, RV_OP_LH, RV_OP_LW, RV_OP_LD,
                RV_OP_LBU, RV_OP_LHU, RV_OP_LWU, RV_OP_INVALID);
      break;
      
    case OP_STORE:
      dec.imm = S_IMM(inst);
      SET_OP_BY_FUNCT3(0, RV_OP_SB, RV_OP_SH, RV_OP_SW, RV_OP_SD,
                RV_OP_INVALID, RV_OP_INVALID, RV_OP_INVALID, RV_OP_INVALID);
      break;
      
    case OP_IMM:
      dec.imm = I_IMM(inst);
      if (funct3 == 1 || funct3 == 5) {
        DECODE_SHIFT(RV_OP_SLLI, RV_OP_SRLI, RV_OP_SRAI, 25);
      } else {
        SET_OP_BY_FUNCT3(0, RV_OP_ADDI, RV_OP_INVALID, RV_OP_SLTI, RV_OP_SLTIU,
                  RV_OP_XORI, RV_OP_INVALID, RV_OP_ORI, RV_OP_ANDI);
      }
      break;
      
    case OP_IMM_32:
      dec.imm = I_IMM(inst);
      if (funct3 == 1 || funct3 == 5) {
        DECODE_SHIFT(RV_OP_SLLIW, RV_OP_SRLIW, RV_OP_SRAIW, 24);
      } else if (funct3 == 0) {
        dec.opcode = RV_OP_ADDIW;
      } else {
        dec.opcode = RV_OP_INVALID;
      }
      break;
      
    case OP_REG:
      DECODE_R_TYPE(0x00, RV_OP_ADD, RV_OP_SLL, RV_OP_SLT, RV_OP_SLTU,
                RV_OP_XOR, RV_OP_SRL, RV_OP_OR, RV_OP_AND);
      DECODE_R_TYPE(0x20, RV_OP_SUB, RV_OP_INVALID, RV_OP_INVALID, RV_OP_INVALID,
                RV_OP_INVALID, RV_OP_SRA, RV_OP_INVALID, RV_OP_INVALID);
      DECODE_R_TYPE(0x01, RV_OP_MUL, RV_OP_MULH, RV_OP_MULHSU, RV_OP_MULHU,
                RV_OP_DIV, RV_OP_DIVU, RV_OP_REM, RV_OP_REMU);
      break;
      
    case OP_REG_32:
      DECODE_R_TYPE(0x00, RV_OP_ADDW, RV_OP_SLLW, RV_OP_INVALID, RV_OP_INVALID,
                RV_OP_INVALID, RV_OP_SRLW, RV_OP_INVALID, RV_OP_INVALID);
      DECODE_R_TYPE(0x20, RV_OP_SUBW, RV_OP_INVALID, RV_OP_INVALID, RV_OP_INVALID,
                RV_OP_INVALID, RV_OP_SRAW, RV_OP_INVALID, RV_OP_INVALID);
      DECODE_R_TYPE(0x01, RV_OP_MULW, RV_OP_INVALID, RV_OP_INVALID, RV_OP_INVALID,
                RV_OP_DIVW, RV_OP_DIVUW, RV_OP_REMW, RV_OP_REMUW);
      break;
      
    case OP_FENCE:
      dec.opcode = (funct3 == 0) ? RV_OP_FENCE : (funct3 == 1) ? RV_OP_FENCE_I : RV_OP_INVALID;
      break;
      
    case OP_SYSTEM:
      if (funct3 == 0) {
        u32 funct12 = BITS(inst, 31, 20);
        switch (funct12) {
          case 0x000: dec.opcode = RV_OP_ECALL; break;
          case 0x001: dec.opcode = RV_OP_EBREAK; break;
          case 0x002: dec.opcode = RV_OP_URET; break;
          case 0x102: dec.opcode = RV_OP_SRET; break;
          case 0x302: dec.opcode = RV_OP_MRET; break;
          case 0x105: dec.opcode = RV_OP_WFI; break;
          case 0x120: dec.opcode = RV_OP_SFENCE_VMA; break;
          default: dec.opcode = RV_OP_INVALID;
        }
      } else {
        dec.imm = BITS(inst, 31, 20); // CSR address
        SET_OP_BY_FUNCT3(0, RV_OP_INVALID, RV_OP_CSRRW, RV_OP_CSRRS, RV_OP_CSRRC,
                  RV_OP_INVALID, RV_OP_CSRRWI, RV_OP_CSRRSI, RV_OP_CSRRCI);
      }
      break;
      
    case OP_AMO:
      if (funct3 == 2) { // 32-bit atomics
        u32 funct5 = BITS(funct7, 6, 2);
        static const rv_opcode_t amo32_ops[] = {
          RV_OP_AMOADD_W, RV_OP_AMOSWAP_W, RV_OP_LR_W, RV_OP_SC_W,
          RV_OP_AMOXOR_W, RV_OP_INVALID, RV_OP_INVALID, RV_OP_INVALID,
          RV_OP_AMOOR_W, RV_OP_INVALID, RV_OP_INVALID, RV_OP_INVALID,
          RV_OP_AMOAND_W, RV_OP_INVALID, RV_OP_INVALID, RV_OP_INVALID,
          RV_OP_AMOMIN_W, RV_OP_INVALID, RV_OP_INVALID, RV_OP_INVALID,
          RV_OP_AMOMAX_W, RV_OP_INVALID, RV_OP_INVALID, RV_OP_INVALID,
          RV_OP_AMOMINU_W, RV_OP_INVALID, RV_OP_INVALID, RV_OP_INVALID,
          RV_OP_AMOMAXU_W
        };
        dec.opcode = (funct5 < sizeof(amo32_ops)/sizeof(amo32_ops[0])) ? 
               amo32_ops[funct5] : RV_OP_INVALID;
      } else if (funct3 == 3) { // 64-bit atomics
        u32 funct5 = BITS(funct7, 6, 2);
        static const rv_opcode_t amo64_ops[] = {
          RV_OP_AMOADD_D, RV_OP_AMOSWAP_D, RV_OP_LR_D, RV_OP_SC_D,
          RV_OP_AMOXOR_D, RV_OP_INVALID, RV_OP_INVALID, RV_OP_INVALID,
          RV_OP_AMOOR_D, RV_OP_INVALID, RV_OP_INVALID, RV_OP_INVALID,
          RV_OP_AMOAND_D, RV_OP_INVALID, RV_OP_INVALID, RV_OP_INVALID,
          RV_OP_AMOMIN_D, RV_OP_INVALID, RV_OP_INVALID, RV_OP_INVALID,
          RV_OP_AMOMAX_D, RV_OP_INVALID, RV_OP_INVALID, RV_OP_INVALID,
          RV_OP_AMOMINU_D, RV_OP_INVALID, RV_OP_INVALID, RV_OP_INVALID,
          RV_OP_AMOMAXU_D
        };
        dec.opcode = (funct5 < sizeof(amo64_ops)/sizeof(amo64_ops[0])) ? 
               amo64_ops[funct5] : RV_OP_INVALID;
      } else {
        dec.opcode = RV_OP_INVALID;
      }
      break;
      
    case OP_FP_LOAD:
      dec.imm = I_IMM(inst);
      dec.opcode = (funct3 == 2) ? RV_OP_FLW : (funct3 == 3) ? RV_OP_FLD : RV_OP_INVALID;
      break;
      
    case OP_FP_STORE:
      dec.imm = S_IMM(inst);
      dec.opcode = (funct3 == 2) ? RV_OP_FSW : (funct3 == 3) ? RV_OP_FSD : RV_OP_INVALID;
      break;
      
    case OP_FP_MADD:
      dec.opcode = (BITS(inst, 26, 25) == 0) ? RV_OP_FMADD_S : 
             (BITS(inst, 26, 25) == 1) ? RV_OP_FMADD_D : RV_OP_INVALID;
      break;
      
    case OP_FP_MSUB:
      dec.opcode = (BITS(inst, 26, 25) == 0) ? RV_OP_FMSUB_S : 
             (BITS(inst, 26, 25) == 1) ? RV_OP_FMSUB_D : RV_OP_INVALID;
      break;
      
    case OP_FP_NMSUB:
      dec.opcode = (BITS(inst, 26, 25) == 0) ? RV_OP_FNMSUB_S : 
             (BITS(inst, 26, 25) == 1) ? RV_OP_FNMSUB_D : RV_OP_INVALID;
      break;
      
    case OP_FP_NMADD:
      dec.opcode = (BITS(inst, 26, 25) == 0) ? RV_OP_FNMADD_S : 
             (BITS(inst, 26, 25) == 1) ? RV_OP_FNMADD_D : RV_OP_INVALID;
      break;
      
    case OP_FP:
      {
        u32 fmt = BITS(inst, 26, 25);
        if (funct7 == 0x00) { // FADD
          dec.opcode = (fmt == 0) ? RV_OP_FADD_S : RV_OP_FADD_D;
        } else if (funct7 == 0x04) { // FSUB
          dec.opcode = (fmt == 0) ? RV_OP_FSUB_S : RV_OP_FSUB_D;
        } else if (funct7 == 0x08) { // FMUL
          dec.opcode = (fmt == 0) ? RV_OP_FMUL_S : RV_OP_FMUL_D;
        } else if (funct7 == 0x0C) { // FDIV
          dec.opcode = (fmt == 0) ? RV_OP_FDIV_S : RV_OP_FDIV_D;
        } else if (funct7 == 0x2C && GET_RS2(inst) == 0) { // FSQRT
          dec.opcode = (fmt == 0) ? RV_OP_FSQRT_S : RV_OP_FSQRT_D;
        } else if (funct7 == 0x10) { // FSGNJ
          static const rv_opcode_t fsgnj_ops[][3] = {
            {RV_OP_FSGNJ_S, RV_OP_FSGNJN_S, RV_OP_FSGNJX_S},
            {RV_OP_FSGNJ_D, RV_OP_FSGNJN_D, RV_OP_FSGNJX_D}
          };
          dec.opcode = (fmt <= 1 && funct3 <= 2) ? fsgnj_ops[fmt][funct3] : RV_OP_INVALID;
        } else if (funct7 == 0x14) { // FMIN/FMAX
          static const rv_opcode_t fminmax_ops[][2] = {
            {RV_OP_FMIN_S, RV_OP_FMAX_S},
            {RV_OP_FMIN_D, RV_OP_FMAX_D}
          };
          dec.opcode = (fmt <= 1 && funct3 <= 1) ? fminmax_ops[fmt][funct3] : RV_OP_INVALID;
        } else if (funct7 == 0x20 && fmt == 1 && GET_RS2(inst) == 0) { // FCVT.S.D
          dec.opcode = RV_OP_FCVT_S_D;
        } else if (funct7 == 0x21 && fmt == 0 && GET_RS2(inst) == 1) { // FCVT.D.S
          dec.opcode = RV_OP_FCVT_D_S;
        } else if (funct7 == 0x50) { // FEQ/FLT/FLE
          static const rv_opcode_t fcmp_ops[][3] = {
            {RV_OP_FLE_S, RV_OP_FLT_S, RV_OP_FEQ_S},
            {RV_OP_FLE_D, RV_OP_FLT_D, RV_OP_FEQ_D}
          };
          dec.opcode = (fmt <= 1 && funct3 <= 2) ? fcmp_ops[fmt][funct3] : RV_OP_INVALID;
        } else if (funct7 == 0x60) { // FCVT.W/WU/L/LU.S/D
          static const rv_opcode_t fcvt_to_int[][4] = {
            {RV_OP_FCVT_W_S, RV_OP_FCVT_WU_S, RV_OP_FCVT_L_S, RV_OP_FCVT_LU_S},
            {RV_OP_FCVT_W_D, RV_OP_FCVT_WU_D, RV_OP_FCVT_L_D, RV_OP_FCVT_LU_D}
          };
          u32 rs2 = GET_RS2(inst);
          dec.opcode = (fmt <= 1 && rs2 <= 3) ? fcvt_to_int[fmt][rs2] : RV_OP_INVALID;
        } else if (funct7 == 0x68) { // FCVT.S/D.W/WU/L/LU
          static const rv_opcode_t fcvt_from_int[][4] = {
            {RV_OP_FCVT_S_W, RV_OP_FCVT_S_WU, RV_OP_FCVT_S_L, RV_OP_FCVT_S_LU},
            {RV_OP_FCVT_D_W, RV_OP_FCVT_D_WU, RV_OP_FCVT_D_L, RV_OP_FCVT_D_LU}
          };
          u32 rs2 = GET_RS2(inst);
          dec.opcode = (fmt <= 1 && rs2 <= 3) ? fcvt_from_int[fmt][rs2] : RV_OP_INVALID;
        } else if (funct7 == 0x70) { // FMV.X.W/FMV.X.D and FCLASS
          if (funct3 == 0 && GET_RS2(inst) == 0) {
            dec.opcode = (fmt == 0) ? RV_OP_FMV_X_W : RV_OP_FMV_X_D;
          } else if (funct3 == 1 && GET_RS2(inst) == 0) {
            dec.opcode = (fmt == 0) ? RV_OP_FCLASS_S : RV_OP_FCLASS_D;
          } else {
            dec.opcode = RV_OP_INVALID;
          }
        } else if (funct7 == 0x78) { // FMV.W.X/FMV.D.X
          if (funct3 == 0 && GET_RS2(inst) == 0) {
            dec.opcode = (fmt == 0) ? RV_OP_FMV_W_X : RV_OP_FMV_D_X;
          } else {
            dec.opcode = RV_OP_INVALID;
          }
        } else {
          dec.opcode = RV_OP_INVALID;
        }
      }
      break;
      
    default:
      dec.opcode = RV_OP_INVALID;
  }
  
  return dec;
}
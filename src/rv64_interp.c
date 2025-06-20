#include "common.h"
#include "rv64gc.h"
#include "rv64_opcodes.h"
#include <math.h>

// Helper macros for common operations
#define REG_RD (rv->x[inst.rd])
#define REG_RS1 (rv->x[inst.rs1])
#define REG_RS2 (rv->x[inst.rs2])
#define FREG_RD (rv->f[inst.rd])
#define FREG_RS1 (rv->f[inst.rs1])
#define FREG_RS2 (rv->f[inst.rs2])
#define FREG_RS3 (rv->f[inst.rs3])

// Address calculation
#define CALC_ADDR(base, offset) ((base) + (offset))

// Sign extension macros
#define SEXT32(x) ((s64)(s32)(x))
#define ZEXT32(x) ((u64)(u32)(x))

// Branch macro
#define BRANCH_IF(cond) do { if (cond) rv->next_pc = rv->pc + inst.imm; } while(0)

// Load operation macro
#define LOAD_OP(bits, type, sext) do { \
  addr = CALC_ADDR(REG_RS1, inst.imm); \
  if (addr & ((bits/8) - 1)) return RV_RES_BAD_ALIGN; \
  u##bits val; \
  res = rv_vread##bits(rv, addr, &val); \
  if (res != RV_RES_OK) return res; \
  REG_RD = sext ? (s64)(s##type)val : (u64)val; \
} while(0)

// Store operation macro
#define STORE_OP(bits) do { \
  addr = CALC_ADDR(REG_RS1, inst.imm); \
  if (addr & ((bits/8) - 1)) return RV_RES_BAD_ALIGN; \
  res = rv_vwrite##bits(rv, addr, REG_RS2); \
  if (res != RV_RES_OK) return res; \
} while(0)

// Floating-point load/store macros
#define FP_LOAD(bits, reg_field) do { \
  addr = CALC_ADDR(REG_RS1, inst.imm); \
  if (addr & ((bits/8) - 1)) return RV_RES_BAD_ALIGN; \
  u##bits val; \
  res = rv_vread##bits(rv, addr, &val); \
  if (res != RV_RES_OK) return res; \
  FREG_RD.reg_field = val; \
} while(0)

#define FP_STORE(bits, reg_field) do { \
  addr = CALC_ADDR(REG_RS1, inst.imm); \
  if (addr & ((bits/8) - 1)) return RV_RES_BAD_ALIGN; \
  res = rv_vwrite##bits(rv, addr, FREG_RS2.reg_field); \
  if (res != RV_RES_OK) return res; \
} while(0)

// ALU operation macros
#define ALU_OP(op) (REG_RD = REG_RS1 op REG_RS2)
#define ALU_IMM_OP(op) (REG_RD = REG_RS1 op inst.imm)
#define ALU_SHIFT_OP(op, mask) (REG_RD = REG_RS1 op (REG_RS2 & mask))
#define ALU_SHIFT_IMM_OP(op, mask) (REG_RD = REG_RS1 op (inst.imm & mask))

// Word operations (32-bit sign-extended)
#define ALU_WORD_OP(expr) (REG_RD = SEXT32(expr))
#define ALU_WORD_SHIFT_OP(op, mask) (REG_RD = SEXT32((u32)REG_RS1 op (REG_RS2 & mask)))
#define ALU_WORD_SHIFT_IMM_OP(op, mask) (REG_RD = SEXT32((u32)REG_RS1 op (inst.imm & mask)))

// CSR operation macro
#define CSR_OP(read_cond, write_expr) do { \
  u64 old = rv_csr_read(rv, inst.imm); \
  if (read_cond) rv_csr_write(rv, inst.imm, write_expr); \
  REG_RD = old; \
} while(0)

// Division operation helpers
#define DIV_CHECK_ZERO(divisor, result) do { \
  if ((divisor) == 0) { REG_RD = (result); break; } \
} while(0)

#define DIV_CHECK_OVERFLOW(dividend, divisor, min_val, result) do { \
  if ((dividend) == (min_val) && (divisor) == -1) { REG_RD = (result); break; } \
} while(0)

// AMO operation macro
#define AMO_OP(bits, op_expr, is_signed) do { \
  addr = REG_RS1; \
  if (addr & ((bits/8) - 1)) return RV_RES_BAD_ALIGN; \
  u##bits val; \
  res = rv_vread##bits(rv, addr, &val); \
  if (res != RV_RES_OK) return res; \
  res = rv_vwrite##bits(rv, addr, op_expr); \
  if (res != RV_RES_OK) return res; \
  REG_RD = is_signed ? SEXT32(val) : (u64)val; \
} while(0)

// Floating-point sign manipulation
#define FP_SIGN_OP(bits, op) do { \
  u##bits sign_mask = 1ULL << ((bits) - 1); \
  u##bits abs_mask = sign_mask - 1; \
  u##bits sign = op; \
  FREG_RD.l = (FREG_RS1.l & abs_mask) | sign; \
} while(0)

// Compressed instruction helpers
#define C_LOAD_OP(bits, type, sext, base_reg) do { \
  addr = rv->x[base_reg] + inst.imm; \
  if (addr & ((bits/8) - 1)) return RV_RES_BAD_ALIGN; \
  u##bits val; \
  res = rv_vread##bits(rv, addr, &val); \
  if (res != RV_RES_OK) return res; \
  REG_RD = sext ? (s64)(s##type)val : (u64)val; \
} while(0)

#define C_STORE_OP(bits, base_reg) do { \
  addr = rv->x[base_reg] + inst.imm; \
  if (addr & ((bits/8) - 1)) return RV_RES_BAD_ALIGN; \
  res = rv_vwrite##bits(rv, addr, REG_RS2); \
  if (res != RV_RES_OK) return res; \
} while(0)

#define C_FP_LOAD(bits, reg_field, base_reg) do { \
  addr = rv->x[base_reg] + inst.imm; \
  if (addr & ((bits/8) - 1)) return RV_RES_BAD_ALIGN; \
  u##bits val; \
  res = rv_vread##bits(rv, addr, &val); \
  if (res != RV_RES_OK) return res; \
  FREG_RD.reg_field = val; \
} while(0)

#define C_FP_STORE(bits, reg_field, base_reg) do { \
  addr = rv->x[base_reg] + inst.imm; \
  if (addr & ((bits/8) - 1)) return RV_RES_BAD_ALIGN; \
  res = rv_vwrite##bits(rv, addr, FREG_RS2.reg_field); \
  if (res != RV_RES_OK) return res; \
} while(0)

rv_res_e rv64_interp(rv_t* rv, rv_inst_t inst) {
  u64 addr;
  rv_res_e res;
  
  switch (inst.opcode) {
    // RV32I/RV64I Base Instructions
    case RV_OP_LUI:
      REG_RD = inst.imm;
      break;
      
    case RV_OP_AUIPC:
      REG_RD = rv->pc + inst.imm;
      break;
      
    case RV_OP_JAL:
      REG_RD = rv->pc + 4;
      rv->next_pc = rv->pc + inst.imm;
      break;
      
    case RV_OP_JALR:
      REG_RD = rv->pc + 4;
      rv->next_pc = (REG_RS1 + inst.imm) & ~1ULL;
      break;
      
    // Branch instructions
    case RV_OP_BEQ: BRANCH_IF(REG_RS1 == REG_RS2); break;
    case RV_OP_BNE: BRANCH_IF(REG_RS1 != REG_RS2); break;
    case RV_OP_BLT: BRANCH_IF((s64)REG_RS1 < (s64)REG_RS2); break;
    case RV_OP_BGE: BRANCH_IF((s64)REG_RS1 >= (s64)REG_RS2); break;
    case RV_OP_BLTU: BRANCH_IF(REG_RS1 < REG_RS2); break;
    case RV_OP_BGEU: BRANCH_IF(REG_RS1 >= REG_RS2); break;
      
    // Load instructions
    case RV_OP_LB: LOAD_OP(8, 8, 1); break;
    case RV_OP_LH: LOAD_OP(16, 16, 1); break;
    case RV_OP_LW: LOAD_OP(32, 32, 1); break;
    case RV_OP_LD: LOAD_OP(64, 64, 0); break;
    case RV_OP_LBU: LOAD_OP(8, 8, 0); break;
    case RV_OP_LHU: LOAD_OP(16, 16, 0); break;
    case RV_OP_LWU: LOAD_OP(32, 32, 0); break;
      
    // Store instructions
    case RV_OP_SB: STORE_OP(8); break;
    case RV_OP_SH: STORE_OP(16); break;
    case RV_OP_SW: STORE_OP(32); break;
    case RV_OP_SD: STORE_OP(64); break;
      
    // ALU immediate instructions
    case RV_OP_ADDI: ALU_IMM_OP(+); break;
    case RV_OP_SLTI: REG_RD = (s64)REG_RS1 < (s64)inst.imm ? 1 : 0; break;
    case RV_OP_SLTIU: REG_RD = REG_RS1 < (u64)inst.imm ? 1 : 0; break;
    case RV_OP_XORI: ALU_IMM_OP(^); break;
    case RV_OP_ORI:  ALU_IMM_OP(|); break;
    case RV_OP_ANDI: ALU_IMM_OP(&); break;
    case RV_OP_SLLI: ALU_SHIFT_IMM_OP(<<, 0x3f); break;
    case RV_OP_SRLI: ALU_SHIFT_IMM_OP(>>, 0x3f); break;
    case RV_OP_SRAI: REG_RD = (s64)REG_RS1 >> (inst.imm & 0x3f); break;
      
    // ALU register instructions
    case RV_OP_ADD: ALU_OP(+); break;
    case RV_OP_SUB: ALU_OP(-); break;
    case RV_OP_SLL: ALU_SHIFT_OP(<<, 0x3f); break;
    case RV_OP_SLT: REG_RD = (s64)REG_RS1 < (s64)REG_RS2 ? 1 : 0; break;
    case RV_OP_SLTU: REG_RD = REG_RS1 < REG_RS2 ? 1 : 0; break;
    case RV_OP_XOR: ALU_OP(^); break;
    case RV_OP_SRL: ALU_SHIFT_OP(>>, 0x3f); break;
    case RV_OP_SRA: REG_RD = (s64)REG_RS1 >> (REG_RS2 & 0x3f); break;
    case RV_OP_OR:  ALU_OP(|); break;
    case RV_OP_AND: ALU_OP(&); break;
      
    // System instructions
    case RV_OP_FENCE:
    case RV_OP_FENCE_I:
      // No-op for now
      break;
      
    case RV_OP_ECALL:
      // In usermode emulation, handle syscalls directly
      if (rv->syscall_ctx && rv->priv == RV_PRIV_USER) {
        extern s64 handle_linux_syscall(rv_t* rv, void* ctx);
        s64 ret = handle_linux_syscall(rv, rv->syscall_ctx);
        rv->x[10] = ret;  // Return value in a0
        rv->pc = rv->next_pc;  // Continue execution after syscall
        return RV_RES_OK;
      }
      return rv_trap(rv, 8 + rv->priv, 0);
      
    case RV_OP_EBREAK:
      return rv_trap(rv, RV_EBP, rv->pc);
      
    case RV_OP_URET:
    case RV_OP_SRET:
    case RV_OP_MRET:
      // TODO: Implement privilege return instructions
      return RV_RES_BAD;
      
    case RV_OP_WFI:
      return RV_RES_TRAP_WFI;
      
    case RV_OP_SFENCE_VMA:
      rv->tlb_valid = 0; // Invalidate TLB
      break;
      
    // CSR instructions
    case RV_OP_CSRRW: CSR_OP(1, REG_RS1); break;
    case RV_OP_CSRRS: CSR_OP(inst.rs1 != 0, old | REG_RS1); break;
    case RV_OP_CSRRC: CSR_OP(inst.rs1 != 0, old & ~REG_RS1); break;
    case RV_OP_CSRRWI: CSR_OP(1, inst.rs1); break;
    case RV_OP_CSRRSI: CSR_OP(inst.rs1 != 0, old | inst.rs1); break;
    case RV_OP_CSRRCI: CSR_OP(inst.rs1 != 0, old & ~inst.rs1); break;
      
    // RV64I specific instructions
    case RV_OP_ADDIW: ALU_WORD_OP(REG_RS1 + inst.imm); break;
    case RV_OP_SLLIW: ALU_WORD_OP(REG_RS1 << (inst.imm & 0x1f)); break;
    case RV_OP_SRLIW: ALU_WORD_SHIFT_IMM_OP(>>, 0x1f); break;
    case RV_OP_SRAIW: REG_RD = SEXT32((s32)REG_RS1 >> (inst.imm & 0x1f)); break;
    case RV_OP_ADDW: ALU_WORD_OP(REG_RS1 + REG_RS2); break;
    case RV_OP_SUBW: ALU_WORD_OP(REG_RS1 - REG_RS2); break;
    case RV_OP_SLLW: ALU_WORD_OP(REG_RS1 << (REG_RS2 & 0x1f)); break;
    case RV_OP_SRLW: ALU_WORD_SHIFT_OP(>>, 0x1f); break;
    case RV_OP_SRAW: REG_RD = SEXT32((s32)REG_RS1 >> (REG_RS2 & 0x1f)); break;
      
    // M extension instructions
    case RV_OP_MUL:  ALU_OP(*); break;
    case RV_OP_MULH:  REG_RD = (u64)(((s128)(s64)REG_RS1 * (s128)(s64)REG_RS2) >> 64); break;
    case RV_OP_MULHSU: REG_RD = (u64)(((s128)(s64)REG_RS1 * (u128)REG_RS2) >> 64); break;
    case RV_OP_MULHU: REG_RD = (u64)(((u128)REG_RS1 * (u128)REG_RS2) >> 64); break;
    
    case RV_OP_DIV:
      DIV_CHECK_ZERO(REG_RS2, -1);
      DIV_CHECK_OVERFLOW(REG_RS1, REG_RS2, INT64_MIN, INT64_MIN);
      REG_RD = (s64)REG_RS1 / (s64)REG_RS2;
      break;
      
    case RV_OP_DIVU:
      DIV_CHECK_ZERO(REG_RS2, UINT64_MAX);
      REG_RD = REG_RS1 / REG_RS2;
      break;
      
    case RV_OP_REM:
      DIV_CHECK_ZERO(REG_RS2, REG_RS1);
      DIV_CHECK_OVERFLOW(REG_RS1, REG_RS2, INT64_MIN, 0);
      REG_RD = (s64)REG_RS1 % (s64)REG_RS2;
      break;
      
    case RV_OP_REMU:
      DIV_CHECK_ZERO(REG_RS2, REG_RS1);
      REG_RD = REG_RS1 % REG_RS2;
      break;
      
    case RV_OP_MULW: ALU_WORD_OP((s32)REG_RS1 * (s32)REG_RS2); break;
    
    case RV_OP_DIVW:
      DIV_CHECK_ZERO((s32)REG_RS2, -1);
      DIV_CHECK_OVERFLOW((s32)REG_RS1, (s32)REG_RS2, INT32_MIN, SEXT32(INT32_MIN));
      REG_RD = SEXT32((s32)REG_RS1 / (s32)REG_RS2);
      break;
      
    case RV_OP_DIVUW:
      DIV_CHECK_ZERO((u32)REG_RS2, -1);
      REG_RD = SEXT32((u32)REG_RS1 / (u32)REG_RS2);
      break;
      
    case RV_OP_REMW:
      DIV_CHECK_ZERO((s32)REG_RS2, SEXT32(REG_RS1));
      DIV_CHECK_OVERFLOW((s32)REG_RS1, (s32)REG_RS2, INT32_MIN, 0);
      REG_RD = SEXT32((s32)REG_RS1 % (s32)REG_RS2);
      break;
      
    case RV_OP_REMUW:
      DIV_CHECK_ZERO((u32)REG_RS2, SEXT32(REG_RS1));
      REG_RD = SEXT32((u32)REG_RS1 % (u32)REG_RS2);
      break;
      
    // A extension instructions (32-bit)
    case RV_OP_LR_W:
      addr = REG_RS1;
      if (addr & 3) return RV_RES_BAD_ALIGN;
      {
        u32 val;
        res = rv_vread32(rv, addr, &val);
        if (res != RV_RES_OK) return res;
        REG_RD = SEXT32(val);
        rv->reservation_set = true;
        rv->reservation_addr = addr;
      }
      break;
      
    case RV_OP_SC_W:
      addr = REG_RS1;
      if (addr & 3) return RV_RES_BAD_ALIGN;
      if (rv->reservation_set && rv->reservation_addr == addr) {
        res = rv_vwrite32(rv, addr, REG_RS2);
        if (res != RV_RES_OK) return res;
        REG_RD = 0; // Success
        rv->reservation_set = false;
      } else {
        REG_RD = 1; // Failure
      }
      break;
      
    // AMO 32-bit operations
    case RV_OP_AMOSWAP_W: AMO_OP(32, REG_RS2, 1); break;
    case RV_OP_AMOADD_W: AMO_OP(32, val + (u32)REG_RS2, 1); break;
    case RV_OP_AMOXOR_W: AMO_OP(32, val ^ (u32)REG_RS2, 1); break;
    case RV_OP_AMOAND_W: AMO_OP(32, val & (u32)REG_RS2, 1); break;
    case RV_OP_AMOOR_W:  AMO_OP(32, val | (u32)REG_RS2, 1); break;
    case RV_OP_AMOMIN_W: AMO_OP(32, ((s32)val < (s32)REG_RS2) ? val : (s32)REG_RS2, 1); break;
    case RV_OP_AMOMAX_W: AMO_OP(32, ((s32)val > (s32)REG_RS2) ? val : (s32)REG_RS2, 1); break;
    case RV_OP_AMOMINU_W: AMO_OP(32, (val < (u32)REG_RS2) ? val : (u32)REG_RS2, 1); break;
    case RV_OP_AMOMAXU_W: AMO_OP(32, (val > (u32)REG_RS2) ? val : (u32)REG_RS2, 1); break;
      
    // 64-bit atomic instructions
    case RV_OP_LR_D:
      addr = REG_RS1;
      if (addr & 7) return RV_RES_BAD_ALIGN;
      {
        u64 val;
        res = rv_vread64(rv, addr, &val);
        if (res != RV_RES_OK) return res;
        REG_RD = val;
        rv->reservation_set = true;
        rv->reservation_addr = addr;
      }
      break;
      
    case RV_OP_SC_D:
      addr = REG_RS1;
      if (addr & 7) return RV_RES_BAD_ALIGN;
      if (rv->reservation_set && rv->reservation_addr == addr) {
        res = rv_vwrite64(rv, addr, REG_RS2);
        if (res != RV_RES_OK) return res;
        REG_RD = 0; // Success
        rv->reservation_set = false;
      } else {
        REG_RD = 1; // Failure
      }
      break;
      
    // AMO 64-bit operations
    case RV_OP_AMOSWAP_D: AMO_OP(64, REG_RS2, 0); break;
    case RV_OP_AMOADD_D: AMO_OP(64, val + REG_RS2, 0); break;
    case RV_OP_AMOXOR_D: AMO_OP(64, val ^ REG_RS2, 0); break;
    case RV_OP_AMOAND_D: AMO_OP(64, val & REG_RS2, 0); break;
    case RV_OP_AMOOR_D:  AMO_OP(64, val | REG_RS2, 0); break;
    case RV_OP_AMOMIN_D: AMO_OP(64, ((s64)val < (s64)REG_RS2) ? val : REG_RS2, 0); break;
    case RV_OP_AMOMAX_D: AMO_OP(64, ((s64)val > (s64)REG_RS2) ? val : REG_RS2, 0); break;
    case RV_OP_AMOMINU_D: AMO_OP(64, (val < REG_RS2) ? val : REG_RS2, 0); break;
    case RV_OP_AMOMAXU_D: AMO_OP(64, (val > REG_RS2) ? val : REG_RS2, 0); break;
      
    // F extension (single-precision floating-point)
    case RV_OP_FLW: FP_LOAD(32, w); break;
    case RV_OP_FSW: FP_STORE(32, w); break;
    
    case RV_OP_FMADD_S: FREG_RD.f = FREG_RS1.f * FREG_RS2.f + FREG_RS3.f; break;
    case RV_OP_FMSUB_S: FREG_RD.f = FREG_RS1.f * FREG_RS2.f - FREG_RS3.f; break;
    case RV_OP_FNMSUB_S: FREG_RD.f = -(FREG_RS1.f * FREG_RS2.f) + FREG_RS3.f; break;
    case RV_OP_FNMADD_S: FREG_RD.f = -(FREG_RS1.f * FREG_RS2.f) - FREG_RS3.f; break;
    
    case RV_OP_FADD_S: FREG_RD.f = FREG_RS1.f + FREG_RS2.f; break;
    case RV_OP_FSUB_S: FREG_RD.f = FREG_RS1.f - FREG_RS2.f; break;
    case RV_OP_FMUL_S: FREG_RD.f = FREG_RS1.f * FREG_RS2.f; break;
    case RV_OP_FDIV_S: FREG_RD.f = FREG_RS1.f / FREG_RS2.f; break;
    case RV_OP_FSQRT_S: FREG_RD.f = sqrtf(FREG_RS1.f); break;
    
    case RV_OP_FSGNJ_S: FP_SIGN_OP(32, FREG_RS2.w & 0x80000000); break;
    case RV_OP_FSGNJN_S: FP_SIGN_OP(32, (~FREG_RS2.w) & 0x80000000); break;
    case RV_OP_FSGNJX_S: FP_SIGN_OP(32, (FREG_RS1.w ^ FREG_RS2.w) & 0x80000000); break;
    
    case RV_OP_FMIN_S: FREG_RD.f = fminf(FREG_RS1.f, FREG_RS2.f); break;
    case RV_OP_FMAX_S: FREG_RD.f = fmaxf(FREG_RS1.f, FREG_RS2.f); break;
    
    case RV_OP_FCVT_W_S: REG_RD = SEXT32((s32)FREG_RS1.f); break;
    case RV_OP_FCVT_WU_S: REG_RD = SEXT32((u32)FREG_RS1.f); break;
    case RV_OP_FMV_X_W:  REG_RD = SEXT32(FREG_RS1.w); break;
    
    case RV_OP_FEQ_S: REG_RD = (FREG_RS1.f == FREG_RS2.f) ? 1 : 0; break;
    case RV_OP_FLT_S: REG_RD = (FREG_RS1.f < FREG_RS2.f) ? 1 : 0; break;
    case RV_OP_FLE_S: REG_RD = (FREG_RS1.f <= FREG_RS2.f) ? 1 : 0; break;
    
    case RV_OP_FCLASS_S:
      {
        u32 val = FREG_RS1.w;
        u32 cls = 0;
        if (val == 0x80000000) cls = 0x001; // -0
        else if (val == 0x00000000) cls = 0x010; // +0
        else if ((val & 0x7f800000) == 0x7f800000) {
          if (val & 0x007fffff) cls = (val & 0x80000000) ? 0x100 : 0x200; // NaN
          else cls = (val & 0x80000000) ? 0x002 : 0x080; // Inf
        } else if ((val & 0x7f800000) == 0) {
          cls = (val & 0x80000000) ? 0x004 : 0x040; // Subnormal
        } else {
          cls = (val & 0x80000000) ? 0x008 : 0x020; // Normal
        }
        REG_RD = cls;
      }
      break;
      
    case RV_OP_FCVT_S_W: FREG_RD.f = (f32)(s32)REG_RS1; break;
    case RV_OP_FCVT_S_WU: FREG_RD.f = (f32)(u32)REG_RS1; break;
    case RV_OP_FMV_W_X:  FREG_RD.w = (u32)REG_RS1; break;
    
    case RV_OP_FCVT_L_S: REG_RD = (s64)FREG_RS1.f; break;
    case RV_OP_FCVT_LU_S: REG_RD = (u64)FREG_RS1.f; break;
    case RV_OP_FCVT_S_L: FREG_RD.f = (f32)(s64)REG_RS1; break;
    case RV_OP_FCVT_S_LU: FREG_RD.f = (f32)(u64)REG_RS1; break;
      
    // D extension (double-precision floating-point)
    case RV_OP_FLD: FP_LOAD(64, l); break;
    case RV_OP_FSD: FP_STORE(64, l); break;
    
    case RV_OP_FMADD_D: FREG_RD.d = FREG_RS1.d * FREG_RS2.d + FREG_RS3.d; break;
    case RV_OP_FMSUB_D: FREG_RD.d = FREG_RS1.d * FREG_RS2.d - FREG_RS3.d; break;
    case RV_OP_FNMSUB_D: FREG_RD.d = -(FREG_RS1.d * FREG_RS2.d) + FREG_RS3.d; break;
    case RV_OP_FNMADD_D: FREG_RD.d = -(FREG_RS1.d * FREG_RS2.d) - FREG_RS3.d; break;
    
    case RV_OP_FADD_D: FREG_RD.d = FREG_RS1.d + FREG_RS2.d; break;
    case RV_OP_FSUB_D: FREG_RD.d = FREG_RS1.d - FREG_RS2.d; break;
    case RV_OP_FMUL_D: FREG_RD.d = FREG_RS1.d * FREG_RS2.d; break;
    case RV_OP_FDIV_D: FREG_RD.d = FREG_RS1.d / FREG_RS2.d; break;
    case RV_OP_FSQRT_D: FREG_RD.d = sqrt(FREG_RS1.d); break;
    
    case RV_OP_FSGNJ_D: FP_SIGN_OP(64, FREG_RS2.l & 0x8000000000000000ULL); break;
    case RV_OP_FSGNJN_D: FP_SIGN_OP(64, (~FREG_RS2.l) & 0x8000000000000000ULL); break;
    case RV_OP_FSGNJX_D: FP_SIGN_OP(64, (FREG_RS1.l ^ FREG_RS2.l) & 0x8000000000000000ULL); break;
    
    case RV_OP_FMIN_D: FREG_RD.d = fmin(FREG_RS1.d, FREG_RS2.d); break;
    case RV_OP_FMAX_D: FREG_RD.d = fmax(FREG_RS1.d, FREG_RS2.d); break;
    
    case RV_OP_FCVT_S_D: FREG_RD.f = (f32)FREG_RS1.d; break;
    case RV_OP_FCVT_D_S: FREG_RD.d = (f64)FREG_RS1.f; break;
    
    case RV_OP_FEQ_D: REG_RD = (FREG_RS1.d == FREG_RS2.d) ? 1 : 0; break;
    case RV_OP_FLT_D: REG_RD = (FREG_RS1.d < FREG_RS2.d) ? 1 : 0; break;
    case RV_OP_FLE_D: REG_RD = (FREG_RS1.d <= FREG_RS2.d) ? 1 : 0; break;
    
    case RV_OP_FCLASS_D:
      {
        u64 val = FREG_RS1.l;
        u32 cls = 0;
        if (val == 0x8000000000000000ULL) cls = 0x001; // -0
        else if (val == 0x0000000000000000ULL) cls = 0x010; // +0
        else if ((val & 0x7ff0000000000000ULL) == 0x7ff0000000000000ULL) {
          if (val & 0x000fffffffffffffULL) cls = (val & 0x8000000000000000ULL) ? 0x100 : 0x200; // NaN
          else cls = (val & 0x8000000000000000ULL) ? 0x002 : 0x080; // Inf
        } else if ((val & 0x7ff0000000000000ULL) == 0) {
          cls = (val & 0x8000000000000000ULL) ? 0x004 : 0x040; // Subnormal
        } else {
          cls = (val & 0x8000000000000000ULL) ? 0x008 : 0x020; // Normal
        }
        REG_RD = cls;
      }
      break;
      
    case RV_OP_FCVT_W_D: REG_RD = SEXT32((s32)FREG_RS1.d); break;
    case RV_OP_FCVT_WU_D: REG_RD = SEXT32((u32)FREG_RS1.d); break;
    case RV_OP_FCVT_D_W: FREG_RD.d = (f64)(s32)REG_RS1; break;
    case RV_OP_FCVT_D_WU: FREG_RD.d = (f64)(u32)REG_RS1; break;
    
    case RV_OP_FCVT_L_D: REG_RD = (s64)FREG_RS1.d; break;
    case RV_OP_FCVT_LU_D: REG_RD = (u64)FREG_RS1.d; break;
    case RV_OP_FMV_X_D:  REG_RD = FREG_RS1.l; break;
    case RV_OP_FCVT_D_L: FREG_RD.d = (f64)(s64)REG_RS1; break;
    case RV_OP_FCVT_D_LU: FREG_RD.d = (f64)(u64)REG_RS1; break;
    case RV_OP_FMV_D_X:  FREG_RD.l = REG_RS1; break;
      
    // C extension (compressed instructions)
    case RV_OP_C_ADDI4SPN: REG_RD = rv->x[2] + inst.imm; break;
    case RV_OP_C_FLD:   C_FP_LOAD(64, l, inst.rs1); break;
    case RV_OP_C_LW:    C_LOAD_OP(32, 32, 1, inst.rs1); break;
    case RV_OP_C_FLW:   C_FP_LOAD(32, w, inst.rs1); break;
    case RV_OP_C_LD:    C_LOAD_OP(64, 64, 0, inst.rs1); break;
    case RV_OP_C_FSD:   C_FP_STORE(64, l, inst.rs1); break;
    case RV_OP_C_SW:    C_STORE_OP(32, inst.rs1); break;
    case RV_OP_C_FSW:   C_FP_STORE(32, w, inst.rs1); break;
    case RV_OP_C_SD:    C_STORE_OP(64, inst.rs1); break;
    case RV_OP_C_NOP:   break;
    case RV_OP_C_ADDI:   REG_RD = REG_RD + inst.imm; break;
    case RV_OP_C_JAL:   rv->x[1] = rv->pc + 2; rv->next_pc = rv->pc + inst.imm; break;
    case RV_OP_C_ADDIW:  REG_RD = SEXT32(REG_RD + inst.imm); break;
    case RV_OP_C_LI:    REG_RD = inst.imm; break;
    case RV_OP_C_ADDI16SP: rv->x[2] = rv->x[2] + inst.imm; break;
    case RV_OP_C_LUI:   REG_RD = inst.imm; break;
    case RV_OP_C_SRLI:   REG_RD = REG_RD >> (inst.imm & 0x3f); break;
    case RV_OP_C_SRAI:   REG_RD = (s64)REG_RD >> (inst.imm & 0x3f); break;
    case RV_OP_C_ANDI:   REG_RD = REG_RD & inst.imm; break;
    case RV_OP_C_SUB:   REG_RD = REG_RD - REG_RS2; break;
    case RV_OP_C_XOR:   REG_RD = REG_RD ^ REG_RS2; break;
    case RV_OP_C_OR:    REG_RD = REG_RD | REG_RS2; break;
    case RV_OP_C_AND:   REG_RD = REG_RD & REG_RS2; break;
    case RV_OP_C_SUBW:   REG_RD = SEXT32(REG_RD - REG_RS2); break;
    case RV_OP_C_ADDW:   REG_RD = SEXT32(REG_RD + REG_RS2); break;
    case RV_OP_C_J:    rv->next_pc = rv->pc + inst.imm; break;
    case RV_OP_C_BEQZ:   BRANCH_IF(REG_RS1 == 0); break;
    case RV_OP_C_BNEZ:   BRANCH_IF(REG_RS1 != 0); break;
    case RV_OP_C_SLLI:   REG_RD = REG_RD << (inst.imm & 0x3f); break;
    case RV_OP_C_FLDSP:  C_FP_LOAD(64, l, 2); break;
    case RV_OP_C_LWSP:   C_LOAD_OP(32, 32, 1, 2); break;
    case RV_OP_C_FLWSP:  C_FP_LOAD(32, w, 2); break;
    case RV_OP_C_LDSP:   C_LOAD_OP(64, 64, 0, 2); break;
    case RV_OP_C_JR:    rv->next_pc = REG_RS1 & ~1ULL; break;
    case RV_OP_C_MV:    REG_RD = REG_RS2; break;
    case RV_OP_C_EBREAK:  return rv_trap(rv, RV_EBP, rv->pc);
    case RV_OP_C_JALR:   rv->x[1] = rv->pc + 2; rv->next_pc = REG_RS1 & ~1ULL; break;
    case RV_OP_C_ADD:   REG_RD = REG_RD + REG_RS2; break;
    case RV_OP_C_FSDSP:  C_FP_STORE(64, l, 2); break;
    case RV_OP_C_SWSP:   C_STORE_OP(32, 2); break;
    case RV_OP_C_FSWSP:  C_FP_STORE(32, w, 2); break;
    case RV_OP_C_SDSP:   C_STORE_OP(64, 2); break;
      
    case RV_OP_INVALID:
    default:
      return rv_trap(rv, RV_EILL, inst.raw);
  }
  
  return RV_RES_OK;
}
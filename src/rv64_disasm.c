#include <stdio.h>
#include "rv64gc.h"
#include "rv64_opcodes.h"

// Register names
static const char* const reg_names[32] = {
  "zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2",
  "s0", "s1", "a0", "a1", "a2", "a3", "a4", "a5",
  "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7",
  "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6"
};

// Floating-point register names
static const char* const freg_names[32] = {
  "ft0", "ft1", "ft2", "ft3", "ft4", "ft5", "ft6", "ft7",
  "fs0", "fs1", "fa0", "fa1", "fa2", "fa3", "fa4", "fa5",
  "fa6", "fa7", "fs2", "fs3", "fs4", "fs5", "fs6", "fs7",
  "fs8", "fs9", "fs10", "fs11", "ft8", "ft9", "ft10", "ft11"
};

// Macro for common instruction formats
#define FMT_R(op) snprintf(buf, buflen, "%s %s, %s, %s", op, R(inst.rd), R(inst.rs1), R(inst.rs2))
#define FMT_I(op) snprintf(buf, buflen, "%s %s, %s, %lld", op, R(inst.rd), R(inst.rs1), (long long)inst.imm)
#define FMT_S(op) snprintf(buf, buflen, "%s %s, %lld(%s)", op, R(inst.rs2), (long long)inst.imm, R(inst.rs1))
#define FMT_B(op) snprintf(buf, buflen, "%s %s, %s, 0x%llx", op, R(inst.rs1), R(inst.rs2), (unsigned long long)(pc + inst.imm))
#define FMT_U(op) snprintf(buf, buflen, "%s %s, 0x%llx", op, R(inst.rd), (unsigned long long)(inst.imm >> 12))
#define FMT_J(op) snprintf(buf, buflen, "%s %s, 0x%llx", op, R(inst.rd), (unsigned long long)(pc + inst.imm))

// Floating-point formats
#define FMT_FR(op) snprintf(buf, buflen, "%s %s, %s, %s", op, F(inst.rd), F(inst.rs1), F(inst.rs2))
#define FMT_FI(op) snprintf(buf, buflen, "%s %s, %lld(%s)", op, F(inst.rd), (long long)inst.imm, R(inst.rs1))
#define FMT_FS(op) snprintf(buf, buflen, "%s %s, %lld(%s)", op, F(inst.rs2), (long long)inst.imm, R(inst.rs1))
#define FMT_F4(op) snprintf(buf, buflen, "%s %s, %s, %s, %s", op, F(inst.rd), F(inst.rs1), F(inst.rs2), F(inst.rs3))

// Register name accessors
#define R(x) reg_names[x]
#define F(x) freg_names[x]

// CSR instruction format
#define FMT_CSR(op) do { \
  if (inst.rs1 < 16) \
    snprintf(buf, buflen, "%si %s, 0x%llx, %lld", op, R(inst.rd), (unsigned long long)inst.imm, (long long)inst.rs1); \
  else \
    snprintf(buf, buflen, "%s %s, 0x%llx, %s", op, R(inst.rd), (unsigned long long)inst.imm, R(inst.rs1)); \
} while(0)

// Compressed instruction formats
#define FMT_CI(op) snprintf(buf, buflen, "%s %s, %lld", op, R(inst.rd), (long long)inst.imm)
#define FMT_CL(op) snprintf(buf, buflen, "%s %s, %lld(%s)", op, R(inst.rd), (long long)inst.imm, R(inst.rs1))
#define FMT_CS(op) snprintf(buf, buflen, "%s %s, %lld(%s)", op, R(inst.rs2), (long long)inst.imm, R(inst.rs1))
#define FMT_CR(op) snprintf(buf, buflen, "%s %s, %s", op, R(inst.rd), R(inst.rs2))
#define FMT_CB(op) snprintf(buf, buflen, "%s %s, 0x%llx", op, R(inst.rs1), (unsigned long long)(pc + inst.imm))
#define FMT_CJ(op) snprintf(buf, buflen, "%s 0x%llx", op, (unsigned long long)(pc + inst.imm))

void rv64_disasm(rv_inst_t inst, u64 pc, char* buf, int buflen) {
  // Default to showing raw instruction
  snprintf(buf, buflen, "unknown 0x%08x", inst.raw);
  
  switch (inst.opcode) {
    // RV32I/RV64I Base Integer Instructions
    case RV_OP_LUI:   FMT_U("lui"); break;
    case RV_OP_AUIPC:  FMT_U("auipc"); break;
    case RV_OP_JAL:   FMT_J("jal"); break;
    case RV_OP_JALR:  FMT_I("jalr"); break;
    
    // Branch instructions
    case RV_OP_BEQ:   FMT_B("beq"); break;
    case RV_OP_BNE:   FMT_B("bne"); break;
    case RV_OP_BLT:   FMT_B("blt"); break;
    case RV_OP_BGE:   FMT_B("bge"); break;
    case RV_OP_BLTU:  FMT_B("bltu"); break;
    case RV_OP_BGEU:  FMT_B("bgeu"); break;
    
    // Load instructions
    case RV_OP_LB:   FMT_I("lb"); break;
    case RV_OP_LH:   FMT_I("lh"); break;
    case RV_OP_LW:   FMT_I("lw"); break;
    case RV_OP_LBU:   FMT_I("lbu"); break;
    case RV_OP_LHU:   FMT_I("lhu"); break;
    case RV_OP_LWU:   FMT_I("lwu"); break;
    case RV_OP_LD:   FMT_I("ld"); break;
    
    // Store instructions
    case RV_OP_SB:   FMT_S("sb"); break;
    case RV_OP_SH:   FMT_S("sh"); break;
    case RV_OP_SW:   FMT_S("sw"); break;
    case RV_OP_SD:   FMT_S("sd"); break;
    
    // ALU immediate instructions
    case RV_OP_ADDI:  FMT_I("addi"); break;
    case RV_OP_SLTI:  FMT_I("slti"); break;
    case RV_OP_SLTIU:  FMT_I("sltiu"); break;
    case RV_OP_XORI:  FMT_I("xori"); break;
    case RV_OP_ORI:   FMT_I("ori"); break;
    case RV_OP_ANDI:  FMT_I("andi"); break;
    case RV_OP_SLLI:  FMT_I("slli"); break;
    case RV_OP_SRLI:  FMT_I("srli"); break;
    case RV_OP_SRAI:  FMT_I("srai"); break;
    
    // ALU register instructions
    case RV_OP_ADD:   FMT_R("add"); break;
    case RV_OP_SUB:   FMT_R("sub"); break;
    case RV_OP_SLL:   FMT_R("sll"); break;
    case RV_OP_SLT:   FMT_R("slt"); break;
    case RV_OP_SLTU:  FMT_R("sltu"); break;
    case RV_OP_XOR:   FMT_R("xor"); break;
    case RV_OP_SRL:   FMT_R("srl"); break;
    case RV_OP_SRA:   FMT_R("sra"); break;
    case RV_OP_OR:   FMT_R("or"); break;
    case RV_OP_AND:   FMT_R("and"); break;
    
    // System instructions
    case RV_OP_FENCE:  snprintf(buf, buflen, "fence"); break;
    case RV_OP_FENCE_I: snprintf(buf, buflen, "fence.i"); break;
    case RV_OP_ECALL:  snprintf(buf, buflen, "ecall"); break;
    case RV_OP_EBREAK: snprintf(buf, buflen, "ebreak"); break;
    case RV_OP_URET:  snprintf(buf, buflen, "uret"); break;
    case RV_OP_SRET:  snprintf(buf, buflen, "sret"); break;
    case RV_OP_MRET:  snprintf(buf, buflen, "mret"); break;
    case RV_OP_WFI:   snprintf(buf, buflen, "wfi"); break;
    case RV_OP_SFENCE_VMA: 
      snprintf(buf, buflen, "sfence.vma %s, %s", R(inst.rs1), R(inst.rs2)); 
      break;
    
    // CSR instructions
    case RV_OP_CSRRW:  FMT_CSR("csrrw"); break;
    case RV_OP_CSRRS:  FMT_CSR("csrrs"); break;
    case RV_OP_CSRRC:  FMT_CSR("csrrc"); break;
    case RV_OP_CSRRWI: FMT_CSR("csrrw"); break;
    case RV_OP_CSRRSI: FMT_CSR("csrrs"); break;
    case RV_OP_CSRRCI: FMT_CSR("csrrc"); break;
    
    // RV64I specific instructions
    case RV_OP_ADDIW:  FMT_I("addiw"); break;
    case RV_OP_SLLIW:  FMT_I("slliw"); break;
    case RV_OP_SRLIW:  FMT_I("srliw"); break;
    case RV_OP_SRAIW:  FMT_I("sraiw"); break;
    case RV_OP_ADDW:  FMT_R("addw"); break;
    case RV_OP_SUBW:  FMT_R("subw"); break;
    case RV_OP_SLLW:  FMT_R("sllw"); break;
    case RV_OP_SRLW:  FMT_R("srlw"); break;
    case RV_OP_SRAW:  FMT_R("sraw"); break;
    
    // M extension instructions
    case RV_OP_MUL:   FMT_R("mul"); break;
    case RV_OP_MULH:  FMT_R("mulh"); break;
    case RV_OP_MULHSU: FMT_R("mulhsu"); break;
    case RV_OP_MULHU:  FMT_R("mulhu"); break;
    case RV_OP_DIV:   FMT_R("div"); break;
    case RV_OP_DIVU:  FMT_R("divu"); break;
    case RV_OP_REM:   FMT_R("rem"); break;
    case RV_OP_REMU:  FMT_R("remu"); break;
    case RV_OP_MULW:  FMT_R("mulw"); break;
    case RV_OP_DIVW:  FMT_R("divw"); break;
    case RV_OP_DIVUW:  FMT_R("divuw"); break;
    case RV_OP_REMW:  FMT_R("remw"); break;
    case RV_OP_REMUW:  FMT_R("remuw"); break;
    
    // A extension instructions (32-bit)
    case RV_OP_LR_W:    snprintf(buf, buflen, "lr.w %s, (%s)", R(inst.rd), R(inst.rs1)); break;
    case RV_OP_SC_W:    snprintf(buf, buflen, "sc.w %s, %s, (%s)", R(inst.rd), R(inst.rs2), R(inst.rs1)); break;
    case RV_OP_AMOSWAP_W: snprintf(buf, buflen, "amoswap.w %s, %s, (%s)", R(inst.rd), R(inst.rs2), R(inst.rs1)); break;
    case RV_OP_AMOADD_W:  snprintf(buf, buflen, "amoadd.w %s, %s, (%s)", R(inst.rd), R(inst.rs2), R(inst.rs1)); break;
    case RV_OP_AMOXOR_W:  snprintf(buf, buflen, "amoxor.w %s, %s, (%s)", R(inst.rd), R(inst.rs2), R(inst.rs1)); break;
    case RV_OP_AMOAND_W:  snprintf(buf, buflen, "amoand.w %s, %s, (%s)", R(inst.rd), R(inst.rs2), R(inst.rs1)); break;
    case RV_OP_AMOOR_W:  snprintf(buf, buflen, "amoor.w %s, %s, (%s)", R(inst.rd), R(inst.rs2), R(inst.rs1)); break;
    case RV_OP_AMOMIN_W:  snprintf(buf, buflen, "amomin.w %s, %s, (%s)", R(inst.rd), R(inst.rs2), R(inst.rs1)); break;
    case RV_OP_AMOMAX_W:  snprintf(buf, buflen, "amomax.w %s, %s, (%s)", R(inst.rd), R(inst.rs2), R(inst.rs1)); break;
    case RV_OP_AMOMINU_W: snprintf(buf, buflen, "amominu.w %s, %s, (%s)", R(inst.rd), R(inst.rs2), R(inst.rs1)); break;
    case RV_OP_AMOMAXU_W: snprintf(buf, buflen, "amomaxu.w %s, %s, (%s)", R(inst.rd), R(inst.rs2), R(inst.rs1)); break;
    
    // A extension instructions (64-bit)
    case RV_OP_LR_D:    snprintf(buf, buflen, "lr.d %s, (%s)", R(inst.rd), R(inst.rs1)); break;
    case RV_OP_SC_D:    snprintf(buf, buflen, "sc.d %s, %s, (%s)", R(inst.rd), R(inst.rs2), R(inst.rs1)); break;
    case RV_OP_AMOSWAP_D: snprintf(buf, buflen, "amoswap.d %s, %s, (%s)", R(inst.rd), R(inst.rs2), R(inst.rs1)); break;
    case RV_OP_AMOADD_D:  snprintf(buf, buflen, "amoadd.d %s, %s, (%s)", R(inst.rd), R(inst.rs2), R(inst.rs1)); break;
    case RV_OP_AMOXOR_D:  snprintf(buf, buflen, "amoxor.d %s, %s, (%s)", R(inst.rd), R(inst.rs2), R(inst.rs1)); break;
    case RV_OP_AMOAND_D:  snprintf(buf, buflen, "amoand.d %s, %s, (%s)", R(inst.rd), R(inst.rs2), R(inst.rs1)); break;
    case RV_OP_AMOOR_D:  snprintf(buf, buflen, "amoor.d %s, %s, (%s)", R(inst.rd), R(inst.rs2), R(inst.rs1)); break;
    case RV_OP_AMOMIN_D:  snprintf(buf, buflen, "amomin.d %s, %s, (%s)", R(inst.rd), R(inst.rs2), R(inst.rs1)); break;
    case RV_OP_AMOMAX_D:  snprintf(buf, buflen, "amomax.d %s, %s, (%s)", R(inst.rd), R(inst.rs2), R(inst.rs1)); break;
    case RV_OP_AMOMINU_D: snprintf(buf, buflen, "amominu.d %s, %s, (%s)", R(inst.rd), R(inst.rs2), R(inst.rs1)); break;
    case RV_OP_AMOMAXU_D: snprintf(buf, buflen, "amomaxu.d %s, %s, (%s)", R(inst.rd), R(inst.rs2), R(inst.rs1)); break;
    
    // F extension instructions
    case RV_OP_FLW:    FMT_FI("flw"); break;
    case RV_OP_FSW:    FMT_FS("fsw"); break;
    case RV_OP_FMADD_S:  FMT_F4("fmadd.s"); break;
    case RV_OP_FMSUB_S:  FMT_F4("fmsub.s"); break;
    case RV_OP_FNMSUB_S:  FMT_F4("fnmsub.s"); break;
    case RV_OP_FNMADD_S:  FMT_F4("fnmadd.s"); break;
    case RV_OP_FADD_S:   FMT_FR("fadd.s"); break;
    case RV_OP_FSUB_S:   FMT_FR("fsub.s"); break;
    case RV_OP_FMUL_S:   FMT_FR("fmul.s"); break;
    case RV_OP_FDIV_S:   FMT_FR("fdiv.s"); break;
    case RV_OP_FSQRT_S:  snprintf(buf, buflen, "fsqrt.s %s, %s", F(inst.rd), F(inst.rs1)); break;
    case RV_OP_FSGNJ_S:  FMT_FR("fsgnj.s"); break;
    case RV_OP_FSGNJN_S:  FMT_FR("fsgnjn.s"); break;
    case RV_OP_FSGNJX_S:  FMT_FR("fsgnjx.s"); break;
    case RV_OP_FMIN_S:   FMT_FR("fmin.s"); break;
    case RV_OP_FMAX_S:   FMT_FR("fmax.s"); break;
    case RV_OP_FCVT_W_S:  snprintf(buf, buflen, "fcvt.w.s %s, %s", R(inst.rd), F(inst.rs1)); break;
    case RV_OP_FCVT_WU_S: snprintf(buf, buflen, "fcvt.wu.s %s, %s", R(inst.rd), F(inst.rs1)); break;
    case RV_OP_FMV_X_W:  snprintf(buf, buflen, "fmv.x.w %s, %s", R(inst.rd), F(inst.rs1)); break;
    case RV_OP_FEQ_S:   snprintf(buf, buflen, "feq.s %s, %s, %s", R(inst.rd), F(inst.rs1), F(inst.rs2)); break;
    case RV_OP_FLT_S:   snprintf(buf, buflen, "flt.s %s, %s, %s", R(inst.rd), F(inst.rs1), F(inst.rs2)); break;
    case RV_OP_FLE_S:   snprintf(buf, buflen, "fle.s %s, %s, %s", R(inst.rd), F(inst.rs1), F(inst.rs2)); break;
    case RV_OP_FCLASS_S:  snprintf(buf, buflen, "fclass.s %s, %s", R(inst.rd), F(inst.rs1)); break;
    case RV_OP_FCVT_S_W:  snprintf(buf, buflen, "fcvt.s.w %s, %s", F(inst.rd), R(inst.rs1)); break;
    case RV_OP_FCVT_S_WU: snprintf(buf, buflen, "fcvt.s.wu %s, %s", F(inst.rd), R(inst.rs1)); break;
    case RV_OP_FMV_W_X:  snprintf(buf, buflen, "fmv.w.x %s, %s", F(inst.rd), R(inst.rs1)); break;
    case RV_OP_FCVT_L_S:  snprintf(buf, buflen, "fcvt.l.s %s, %s", R(inst.rd), F(inst.rs1)); break;
    case RV_OP_FCVT_LU_S: snprintf(buf, buflen, "fcvt.lu.s %s, %s", R(inst.rd), F(inst.rs1)); break;
    case RV_OP_FCVT_S_L:  snprintf(buf, buflen, "fcvt.s.l %s, %s", F(inst.rd), R(inst.rs1)); break;
    case RV_OP_FCVT_S_LU: snprintf(buf, buflen, "fcvt.s.lu %s, %s", F(inst.rd), R(inst.rs1)); break;
    
    // D extension instructions
    case RV_OP_FLD:    FMT_FI("fld"); break;
    case RV_OP_FSD:    FMT_FS("fsd"); break;
    case RV_OP_FMADD_D:  FMT_F4("fmadd.d"); break;
    case RV_OP_FMSUB_D:  FMT_F4("fmsub.d"); break;
    case RV_OP_FNMSUB_D:  FMT_F4("fnmsub.d"); break;
    case RV_OP_FNMADD_D:  FMT_F4("fnmadd.d"); break;
    case RV_OP_FADD_D:   FMT_FR("fadd.d"); break;
    case RV_OP_FSUB_D:   FMT_FR("fsub.d"); break;
    case RV_OP_FMUL_D:   FMT_FR("fmul.d"); break;
    case RV_OP_FDIV_D:   FMT_FR("fdiv.d"); break;
    case RV_OP_FSQRT_D:  snprintf(buf, buflen, "fsqrt.d %s, %s", F(inst.rd), F(inst.rs1)); break;
    case RV_OP_FSGNJ_D:  FMT_FR("fsgnj.d"); break;
    case RV_OP_FSGNJN_D:  FMT_FR("fsgnjn.d"); break;
    case RV_OP_FSGNJX_D:  FMT_FR("fsgnjx.d"); break;
    case RV_OP_FMIN_D:   FMT_FR("fmin.d"); break;
    case RV_OP_FMAX_D:   FMT_FR("fmax.d"); break;
    case RV_OP_FCVT_S_D:  snprintf(buf, buflen, "fcvt.s.d %s, %s", F(inst.rd), F(inst.rs1)); break;
    case RV_OP_FCVT_D_S:  snprintf(buf, buflen, "fcvt.d.s %s, %s", F(inst.rd), F(inst.rs1)); break;
    case RV_OP_FEQ_D:   snprintf(buf, buflen, "feq.d %s, %s, %s", R(inst.rd), F(inst.rs1), F(inst.rs2)); break;
    case RV_OP_FLT_D:   snprintf(buf, buflen, "flt.d %s, %s, %s", R(inst.rd), F(inst.rs1), F(inst.rs2)); break;
    case RV_OP_FLE_D:   snprintf(buf, buflen, "fle.d %s, %s, %s", R(inst.rd), F(inst.rs1), F(inst.rs2)); break;
    case RV_OP_FCLASS_D:  snprintf(buf, buflen, "fclass.d %s, %s", R(inst.rd), F(inst.rs1)); break;
    case RV_OP_FCVT_W_D:  snprintf(buf, buflen, "fcvt.w.d %s, %s", R(inst.rd), F(inst.rs1)); break;
    case RV_OP_FCVT_WU_D: snprintf(buf, buflen, "fcvt.wu.d %s, %s", R(inst.rd), F(inst.rs1)); break;
    case RV_OP_FCVT_D_W:  snprintf(buf, buflen, "fcvt.d.w %s, %s", F(inst.rd), R(inst.rs1)); break;
    case RV_OP_FCVT_D_WU: snprintf(buf, buflen, "fcvt.d.wu %s, %s", F(inst.rd), R(inst.rs1)); break;
    case RV_OP_FCVT_L_D:  snprintf(buf, buflen, "fcvt.l.d %s, %s", R(inst.rd), F(inst.rs1)); break;
    case RV_OP_FCVT_LU_D: snprintf(buf, buflen, "fcvt.lu.d %s, %s", R(inst.rd), F(inst.rs1)); break;
    case RV_OP_FMV_X_D:  snprintf(buf, buflen, "fmv.x.d %s, %s", R(inst.rd), F(inst.rs1)); break;
    case RV_OP_FCVT_D_L:  snprintf(buf, buflen, "fcvt.d.l %s, %s", F(inst.rd), R(inst.rs1)); break;
    case RV_OP_FCVT_D_LU: snprintf(buf, buflen, "fcvt.d.lu %s, %s", F(inst.rd), R(inst.rs1)); break;
    case RV_OP_FMV_D_X:  snprintf(buf, buflen, "fmv.d.x %s, %s", F(inst.rd), R(inst.rs1)); break;
    
    // C extension instructions
    case RV_OP_C_ADDI4SPN: FMT_CI("c.addi4spn"); break;
    case RV_OP_C_FLD:   FMT_CL("c.fld"); break;
    case RV_OP_C_LW:    FMT_CL("c.lw"); break;
    case RV_OP_C_FLW:   FMT_CL("c.flw"); break;
    case RV_OP_C_LD:    FMT_CL("c.ld"); break;
    case RV_OP_C_FSD:   FMT_CS("c.fsd"); break;
    case RV_OP_C_SW:    FMT_CS("c.sw"); break;
    case RV_OP_C_FSW:   FMT_CS("c.fsw"); break;
    case RV_OP_C_SD:    FMT_CS("c.sd"); break;
    case RV_OP_C_NOP:   snprintf(buf, buflen, "c.nop"); break;
    case RV_OP_C_ADDI:   FMT_CI("c.addi"); break;
    case RV_OP_C_JAL:   FMT_CJ("c.jal"); break;
    case RV_OP_C_ADDIW:  FMT_CI("c.addiw"); break;
    case RV_OP_C_LI:    FMT_CI("c.li"); break;
    case RV_OP_C_ADDI16SP: FMT_CI("c.addi16sp"); break;
    case RV_OP_C_LUI:   FMT_CI("c.lui"); break;
    case RV_OP_C_SRLI:   FMT_CI("c.srli"); break;
    case RV_OP_C_SRAI:   FMT_CI("c.srai"); break;
    case RV_OP_C_ANDI:   FMT_CI("c.andi"); break;
    case RV_OP_C_SUB:   FMT_CR("c.sub"); break;
    case RV_OP_C_XOR:   FMT_CR("c.xor"); break;
    case RV_OP_C_OR:    FMT_CR("c.or"); break;
    case RV_OP_C_AND:   FMT_CR("c.and"); break;
    case RV_OP_C_SUBW:   FMT_CR("c.subw"); break;
    case RV_OP_C_ADDW:   FMT_CR("c.addw"); break;
    case RV_OP_C_J:    FMT_CJ("c.j"); break;
    case RV_OP_C_BEQZ:   FMT_CB("c.beqz"); break;
    case RV_OP_C_BNEZ:   FMT_CB("c.bnez"); break;
    case RV_OP_C_SLLI:   FMT_CI("c.slli"); break;
    case RV_OP_C_FLDSP:  FMT_CL("c.fldsp"); break;
    case RV_OP_C_LWSP:   FMT_CL("c.lwsp"); break;
    case RV_OP_C_FLWSP:  FMT_CL("c.flwsp"); break;
    case RV_OP_C_LDSP:   FMT_CL("c.ldsp"); break;
    case RV_OP_C_JR:    snprintf(buf, buflen, "c.jr %s", R(inst.rs1)); break;
    case RV_OP_C_MV:    FMT_CR("c.mv"); break;
    case RV_OP_C_EBREAK:  snprintf(buf, buflen, "c.ebreak"); break;
    case RV_OP_C_JALR:   snprintf(buf, buflen, "c.jalr %s", R(inst.rs1)); break;
    case RV_OP_C_ADD:   FMT_CR("c.add"); break;
    case RV_OP_C_FSDSP:  FMT_CS("c.fsdsp"); break;
    case RV_OP_C_SWSP:   FMT_CS("c.swsp"); break;
    case RV_OP_C_FSWSP:  FMT_CS("c.fswsp"); break;
    case RV_OP_C_SDSP:   FMT_CS("c.sdsp"); break;
    
    case RV_OP_INVALID:
      break;
  }
}
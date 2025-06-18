#pragma once

#include "../common.h"

#define MEMORY_SIZE (256 * 1024 * 1024)


// Floating-point register union
typedef union fpr_t {
    struct { f32 f32; u32 pad; };
    f64 f64;
    u64 raw;
} fpr_t;

// CSR state structure
typedef struct csr_state_t {
    // Machine-level CSRs
    u32 mstatus;
    u32 misa;
    u32 medeleg;
    u32 mideleg;
    u32 mie;
    u32 mtvec;
    u32 mcounteren;
    u32 mscratch;
    u32 mepc;
    u32 mcause;
    u32 mtval;
    u32 mip;

    // Supervisor-level CSRs
    u32 sstatus;
    u32 sedeleg;
    u32 sideleg;
    u32 sie;
    u32 stvec;
    u32 scounteren;
    u32 sscratch;
    u32 sepc;
    u32 scause;
    u32 stval;
    u32 sip;
    u32 satp;

    // Performance counters
    u64 cycle;
    u64 time;
    u64 instret;

    // Machine info
    u32 mvendorid;
    u32 marchid;
    u32 mimpid;
    u32 mhartid;

    // Current privilege level
    u32 priv;
} csr_state_t;

// Main processor state
typedef struct rv_t {
    union {
        u32 x[32];
        struct {
            u32 zero, ra,sp,gp,tp,t0,t1,t2,s0,s1,a0,a1,a2,a3,a4,a5,a6,a7,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,t3,t4,t5,t6;
        };
    };

    fpr_t f[32];
    u32 pc;

    u32 reservation_addr;
    int reservation_valid;

    csr_state_t csr;

    u8* ram;
} rv_t;

// Instruction enumeration
typedef enum instr_e {
    // RV32I Base Integer Instruction Set
    ADD, ADDI, SUB, LUI, AUIPC,
    XOR, XORI, OR, ORI, AND, ANDI,
    SLL, SLLI, SRL, SRLI, SRA, SRAI,
    SLT, SLTI, SLTU, SLTIU,
    BEQ, BNE, BLT, BGE, BLTU, BGEU,
    JAL, JALR,
    LB, LH, LW, LBU, LHU,
    SB, SH, SW,
    ECALL, EBREAK, FENCE, FENCE_I,
    MRET, SRET, WFI, SFENCE_VMA,
    CSRRW, CSRRS, CSRRC, CSRRWI, CSRRSI, CSRRCI,

    // RV32M Standard Extension
    MUL, MULH, MULHSU, MULHU, DIV, DIVU, REM, REMU,

    // RV32A Standard Extension
    LR_W, SC_W, AMOSWAP_W, AMOADD_W, AMOXOR_W, AMOAND_W, AMOOR_W,
    AMOMIN_W, AMOMAX_W, AMOMINU_W, AMOMAXU_W,

    // RV32F Standard Extension
    FLW, FSW, FADD_S, FSUB_S, FMUL_S, FDIV_S, FSQRT_S,
    FSGNJ_S, FSGNJN_S, FSGNJX_S, FMIN_S, FMAX_S,
    FCVT_W_S, FCVT_WU_S, FCVT_S_W, FCVT_S_WU,
    FMV_X_W, FMV_W_X, FEQ_S, FLT_S, FLE_S, FCLASS_S,
    FMADD_S, FMSUB_S, FNMSUB_S, FNMADD_S,

    // RV32D Standard Extension
    FLD, FSD, FADD_D, FSUB_D, FMUL_D, FDIV_D, FSQRT_D,
    FSGNJ_D, FSGNJN_D, FSGNJX_D, FMIN_D, FMAX_D,
    FCVT_S_D, FCVT_D_S, FCVT_W_D, FCVT_WU_D, FCVT_D_W, FCVT_D_WU,
    FEQ_D, FLT_D, FLE_D, FCLASS_D,
    FMADD_D, FMSUB_D, FNMSUB_D, FNMADD_D,

    // RV32C Standard Extension
    C_ADDI4SPN, C_LW, C_SW, C_LWSP, C_SWSP,
    C_LW_REG, C_SW_REG, C_J, C_JAL, C_JR, C_JALR,
    C_BEQZ, C_BNEZ, C_LI, C_LUI, C_ADDI, C_ADDI16SP,
    C_SLLI, C_SRLI, C_SRAI, C_ANDI, C_MV, C_ADD,
    C_SUB, C_XOR, C_OR, C_AND, C_EBREAK,
    C_FLW, C_FSW, C_FLWSP, C_FSWSP,
    C_FLD, C_FSD, C_FLDSP, C_FSDSP,

    INSTR_INVALID
} instr_e;

// CSR addresses
#define CSR_USTATUS     0x000
#define CSR_UIE         0x004
#define CSR_UTVEC       0x005
#define CSR_USCRATCH    0x040
#define CSR_UEPC        0x041
#define CSR_UCAUSE      0x042
#define CSR_UTVAL       0x043
#define CSR_UIP         0x044

#define CSR_SSTATUS     0x100
#define CSR_SEDELEG     0x102
#define CSR_SIDELEG     0x103
#define CSR_SIE         0x104
#define CSR_STVEC       0x105
#define CSR_SCOUNTEREN  0x106
#define CSR_SSCRATCH    0x140
#define CSR_SEPC        0x141
#define CSR_SCAUSE      0x142
#define CSR_STVAL       0x143
#define CSR_SIP         0x144
#define CSR_SATP        0x180

#define CSR_MSTATUS     0x300
#define CSR_MISA        0x301
#define CSR_MEDELEG     0x302
#define CSR_MIDELEG     0x303
#define CSR_MIE         0x304
#define CSR_MTVEC       0x305
#define CSR_MCOUNTEREN  0x306
#define CSR_MSCRATCH    0x340
#define CSR_MEPC        0x341
#define CSR_MCAUSE      0x342
#define CSR_MTVAL       0x343
#define CSR_MIP         0x344

#define CSR_CYCLE       0xC00
#define CSR_TIME        0xC01
#define CSR_INSTRET     0xC02
#define CSR_CYCLEH      0xC80
#define CSR_TIMEH       0xC81
#define CSR_INSTRETH    0xC82

#define CSR_MCYCLE      0xB00
#define CSR_MINSTRET    0xB02
#define CSR_MCYCLEH     0xB80
#define CSR_MINSTRETH   0xB82

#define CSR_MVENDORID   0xF11
#define CSR_MARCHID     0xF12
#define CSR_MIMPID      0xF13
#define CSR_MHARTID     0xF14

// Privilege levels
#define PRIV_USER       0
#define PRIV_SUPERVISOR 1
#define PRIV_MACHINE    3

// Status register bits
#define MSTATUS_UIE     (1 << 0)
#define MSTATUS_SIE     (1 << 1)
#define MSTATUS_MIE     (1 << 3)
#define MSTATUS_UPIE    (1 << 4)
#define MSTATUS_SPIE    (1 << 5)
#define MSTATUS_MPIE    (1 << 7)
#define MSTATUS_SPP     (1 << 8)
#define MSTATUS_MPP     (3 << 11)
#define MSTATUS_FS      (3 << 13)
#define MSTATUS_XS      (3 << 15)
#define MSTATUS_MPRV    (1 << 17)
#define MSTATUS_SUM     (1 << 18)
#define MSTATUS_MXR     (1 << 19)
#define MSTATUS_TVM     (1 << 20)
#define MSTATUS_TW      (1 << 21)
#define MSTATUS_TSR     (1 << 22)

// Interrupt bits
#define MIP_USIP        (1 << 0)
#define MIP_SSIP        (1 << 1)
#define MIP_MSIP        (1 << 3)
#define MIP_UTIP        (1 << 4)
#define MIP_STIP        (1 << 5)
#define MIP_MTIP        (1 << 7)
#define MIP_UEIP        (1 << 8)
#define MIP_SEIP        (1 << 9)
#define MIP_MEIP        (1 << 11)

// Exception codes
#define CAUSE_MISALIGNED_FETCH    0
#define CAUSE_FETCH_ACCESS        1
#define CAUSE_ILLEGAL_INSTRUCTION 2
#define CAUSE_BREAKPOINT          3
#define CAUSE_MISALIGNED_LOAD     4
#define CAUSE_LOAD_ACCESS         5
#define CAUSE_MISALIGNED_STORE    6
#define CAUSE_STORE_ACCESS        7
#define CAUSE_USER_ECALL          8
#define CAUSE_SUPERVISOR_ECALL    9
#define CAUSE_MACHINE_ECALL       11
#define CAUSE_FETCH_PAGE_FAULT    12
#define CAUSE_LOAD_PAGE_FAULT     13
#define CAUSE_STORE_PAGE_FAULT    15

// SATP fields for Sv32
#define SATP_PPN        0x003FFFFF
#define SATP_ASID       0x1FC00000
#define SATP_MODE       0x80000000

// Page table entry bits
#define PTE_V           (1 << 0)
#define PTE_R           (1 << 1)
#define PTE_W           (1 << 2)
#define PTE_X           (1 << 3)
#define PTE_U           (1 << 4)
#define PTE_G           (1 << 5)
#define PTE_A           (1 << 6)
#define PTE_D           (1 << 7)


// Core initialization
int rv_init(rv_t* rv);
int rv_free(rv_t* rv);

// Include module headers
#include "mem.h"
#include "csr.h"
#include "interp.h"
#include "disasm.h"


static inline s32 sign_extend(u32 value, int bits) constfunc {
    u32 sign_bit = 1U << (bits - 1);
    return (value ^ sign_bit) - sign_bit;
}

static inline s32 get_i_imm(u32 opcode) constfunc {
    return sign_extend(opcode >> 20, 12);
}

static inline s32 get_s_imm(u32 opcode) constfunc {
    u32 imm = ((opcode >> 25) << 5) | ((opcode >> 7) & 0x1F);
    return sign_extend(imm, 12);
}

static inline s32 get_b_imm(u32 opcode) constfunc {
    u32 imm = ((opcode >> 31) << 12) | (((opcode >> 7) & 1) << 11) |
              (((opcode >> 25) & 0x3F) << 5) | (((opcode >> 8) & 0xF) << 1);
    return sign_extend(imm, 13);
}

static inline s32 get_u_imm(u32 opcode) constfunc {
    return opcode & 0xFFFFF000;
}

static inline s32 get_j_imm(u32 opcode) constfunc {
    u32 imm = ((opcode >> 31) << 20) | (((opcode >> 12) & 0xFF) << 12) |
              (((opcode >> 20) & 1) << 11) | (((opcode >> 21) & 0x3FF) << 1);
    return sign_extend(imm, 21);
}

static inline u32 get_rd(u32 opcode) constfunc { return (opcode >> 7) & 0x1F; }
static inline u32 get_rs1(u32 opcode) constfunc { return (opcode >> 15) & 0x1F; }
static inline u32 get_rs2(u32 opcode) constfunc { return (opcode >> 20) & 0x1F; }
static inline u32 get_funct3(u32 opcode) constfunc { return (opcode >> 12) & 0x7; }
static inline u32 get_funct7(u32 opcode) constfunc { return (opcode >> 25) & 0x7F; }

static inline u32 get_c_rd_rs1(u16 c_inst) constfunc { return (c_inst >> 7) & 0x1F; }
static inline u32 get_c_rs2(u16 c_inst) constfunc { return (c_inst >> 2) & 0x1F; }
static inline u32 get_c_rd_prime(u16 c_inst) constfunc { return ((c_inst >> 2) & 0x7) + 8; }
static inline u32 get_c_rs1_prime(u16 c_inst) constfunc { return ((c_inst >> 7) & 0x7) + 8; }
static inline u32 get_c_rs2_prime(u16 c_inst) constfunc { return ((c_inst >> 2) & 0x7) + 8; }

static inline u32 rv_fclass_s(f32 val) constfunc {
    union { f32 f; u32 i; } u = { .f = val };
    u32 sign = u.i >> 31;
    u32 exp = (u.i >> 23) & 0xFF;
    u32 frac = u.i & 0x7FFFFF;

    if (exp == 0) {
        if (frac == 0) return sign ? (1 << 3) : (1 << 4); // -0 or +0
        else return sign ? (1 << 2) : (1 << 5); // subnormal
    } else if (exp == 0xFF) {
        if (frac == 0) return sign ? (1 << 0) : (1 << 7); // -inf or +inf
        else return (frac & (1 << 22)) ? (1 << 9) : (1 << 8); // qNaN or sNaN
    } else {
        return sign ? (1 << 1) : (1 << 6); // negative or positive normal
    }
}

static inline u32 rv_fclass_d(f64 val) constfunc {
    union { f64 f; u64 i; } u = { .f = val };
    u32 sign = u.i >> 63;
    u32 exp = (u.i >> 52) & 0x7FF;
    u64 frac = u.i & 0xFFFFFFFFFFFFFULL;

    if (exp == 0) {
        if (frac == 0) return sign ? (1 << 3) : (1 << 4); // -0 or +0
        else return sign ? (1 << 2) : (1 << 5); // subnormal
    } else if (exp == 0x7FF) {
        if (frac == 0) return sign ? (1 << 0) : (1 << 7); // -inf or +inf
        else return (frac & (1ULL << 51)) ? (1 << 9) : (1 << 8); // qNaN or sNaN
    } else {
        return sign ? (1 << 1) : (1 << 6); // negative or positive normal
    }
}

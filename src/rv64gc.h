#pragma once

#include "common.h"

#define RV_RESET_VEC 0x80000000ULL

#define RV_PLIC_SIZE 0x4000000
#define RV_PLIC_NSRC 32
#define RV_PLIC_NCTX 1

#define RV_CLINT_SIZE 0x10000

#define MACH_RAM_BASE 0x80000000UL
#define MACH_RAM_SIZE (256UL * 1024UL * 1024UL)
#define MACH_DTB_OFFSET 0x2000000UL
#define MACH_PLIC0_BASE 0xC000000UL
#define MACH_CLINT0_BASE 0x2000000UL
#define MACH_UART0_BASE 0x10000000UL

typedef enum {
    RV_EIALIGN = 0,
    RV_EIFAULT = 1,
    RV_EILL = 2,
    RV_EBP = 3,
    RV_ELALIGN = 4,
    RV_ELFAULT = 5,
    RV_ESALIGN = 6,
    RV_ESFAULT = 7,
    RV_EUECALL = 8,
    RV_ESECALL = 9,
    RV_EMECALL = 11,
    RV_EIPAGE = 12,
    RV_ELPAGE = 13,
    RV_ESPAGE = 15
} rv_exception_e;

typedef enum {
    RV_PRIV_USER = 0,
    RV_PRIV_SUPERVISOR = 1, 
    RV_PRIV_MACHINE = 3
} rv_priv_e;

typedef enum {
    RV_ACCESS_READ = 1,
    RV_ACCESS_WRITE = 2,
    RV_ACCESS_EXEC = 4
} rv_access_e;

typedef enum {
    RV_RES_OK = 0,
    RV_RES_BAD = 1,
    RV_RES_BAD_ALIGN = 2,
    RV_RES_PAGEFAULT = 3,
    RV_RES_TRAP_NONE = 0x80000010,
    RV_RES_TRAP_WFI = 0x80000011
} rv_res_e;

typedef enum {
    RV_CAUSE_SSI = 1,
    RV_CAUSE_MSI = 3,
    RV_CAUSE_STI = 5,
    RV_CAUSE_MTI = 7,
    RV_CAUSE_SEI = 9,
    RV_CAUSE_MEI = 11
} rv_cause_e;

typedef struct {
    u64 sie, stvec, scounteren, sscratch, sepc, scause, stval, sip, satp;
    u64 mvendorid, marchid, mimpid, mhartid;
    u64 mstatus, misa, medeleg, mideleg, mie, mtvec, mcounteren, mstatush;
    u64 mscratch, mepc, mcause, mtval, mip;
    u64 pmpcfg[4], pmpaddr[16];
    u64 mcycle, minstret;
    u64 mcountinhibit;
    u64 mtime, mtimeh, cycle, cycleh;
} rv_csr_t;



typedef struct rv_plic {
    u32 priority[RV_PLIC_NSRC];
    u32 pending[RV_PLIC_NSRC / 32];
    u32 enable[RV_PLIC_NCTX * RV_PLIC_NSRC / 32];
    u32 thresh[RV_PLIC_NCTX];
    u32 claim[RV_PLIC_NCTX];
    u32 claiming[RV_PLIC_NCTX * RV_PLIC_NSRC / 32];
} rv_plic;



typedef struct rv_clint {
    u32 mswi;
    u32 mtimecmp;
    u32 mtimecmph;
} rv_clint;

typedef struct rv_uart {
    u8 tx_ready;
    u8 rx_ready;
    u8 rx_data;
} rv_uart;

typedef union fpr_t {
    f32 f;
    f64 d;
    u32 w;
    u64 l;
} fpr_t;

typedef struct rv_t {
    union {
        u64 x[32];
        struct {
            u64 zero, ra,sp,gp,tp,t0,t1,t2,s0,s1,a0,a1,a2,a3,a4,a5,a6,a7,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,t3,t4,t5,t6;
        };
    };
    fpr_t f[32];  // Floating-point registers
    u64 pc, next_pc;
    rv_priv_e priv;
    rv_csr_t csr;
    bool reservation_set;
    u64 reservation_addr;
    u64 tlb_va, tlb_pte, tlb_valid, tlb_i;
    u8* ram;
    rv_clint clint;
    rv_plic plic;
    rv_uart uart;
    void* syscall_ctx;  // Usermode syscall context
} rv_t;

void rv_init(rv_t* rv);
err_e rv_free(rv_t* rv);
rv_res_e rv_step(rv_t* rv);
void rv_irq(rv_t* rv, u64 cause);

u64 rv_csr_read(rv_t* rv, u32 csr_addr);
void rv_csr_write(rv_t* rv, u32 csr_addr, u64 value);

u8 rv_read8(rv_t* rv, u64 addr);
u16 rv_read16(rv_t* rv, u64 addr);
u32 rv_read32(rv_t* rv, u64 addr);
u64 rv_read64(rv_t* rv, u64 addr);
void rv_write8(rv_t* rv, u64 addr, u8 data);
void rv_write16(rv_t* rv, u64 addr, u16 data);
void rv_write32(rv_t* rv, u64 addr, u32 data);
void rv_write64(rv_t* rv, u64 addr, u64 data);

rv_res_e rv_vread8(rv_t* rv, u64 va, u8 *data);
rv_res_e rv_vread16(rv_t* rv, u64 va, u16 *data);
rv_res_e rv_vread32(rv_t* rv, u64 va, u32 *data);
rv_res_e rv_vread64(rv_t* rv, u64 va, u64 *data);
rv_res_e rv_vwrite8(rv_t* rv, u64 va, u8 data);
rv_res_e rv_vwrite16(rv_t* rv, u64 va, u16 data);
rv_res_e rv_vwrite32(rv_t* rv, u64 va, u32 data);
rv_res_e rv_vwrite64(rv_t* rv, u64 va, u64 data);

rv_res_e rv_vmm(rv_t* rv, u64 va, u64 *pa, rv_access_e access);
rv_res_e rv_trap(rv_t* rv, u64 cause, u64 tval);
void rv_endcvt(u8 *in, u8 *out, u32 width, u32 is_store);

// Include opcode definitions
#include "rv64_opcodes.h"

// Decode, disassemble, and interpret functions
rv_inst_t rv64_decode(u32 inst);
void rv64_disasm(rv_inst_t inst, u64 pc, char* buf, int buflen);
rv_res_e rv64_interp(rv_t* rv, rv_inst_t inst);

// Include hardware emulation functions
#include "rv64_hw.h"

void rv_machine_init(rv_t* rv);
void rv_machine_load_file(const char* path, u8* buf, u64 max_size);
u64 rv_machine_run(rv_t* rv, u64 max_cycles);
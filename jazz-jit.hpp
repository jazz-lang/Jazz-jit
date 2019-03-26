#include <cstdarg>
#include <cstdint>
#include <cstdlib>

namespace jazz_jit
{

static const uintptr_t PAGE_SIZE = 4096;

enum class CondCode
{
  Zero,
  NonZero,
  Equal,
  NotEqual,
  Greater,
  GreaterEq,
  Less,
  LessEq,
  UnsignedGreater,
  UnsignedGreaterEq,
  UnsignedLess,
  UnsignedLessEq,
};

enum class MachineMode
{
  Int8,
  Int32,
  Int64,
  Float32,
  Float64,
  Ptr,
};

enum class Register : int32_t
{
  RAX = 0,
  RCX = 1,
  RDX = 2,
  RBX = 3,
  RSP = 4,
  RBP = 5,
  RSI = 6,
  RDI = 7,
  R8 = 8,
  R9 = 9,
  R10 = 10,
  R11 = 11,
  R12 = 12,
  R13 = 13,
  R14 = 14,
  R15 = 15,
  RIP = 16,
  /// Signals an illegal register.
  kNoRegister = -1,
};

enum class XMMRegister : int32_t
{
  XMM0 = 0,
  XMM1 = 1,
  XMM2 = 2,
  XMM3 = 3,
  XMM4 = 4,
  XMM5 = 5,
  XMM6 = 6,
  XMM7 = 7,
  XMM8 = 8,
  XMM9 = 9,
  XMM10 = 10,
  XMM11 = 11,
  XMM12 = 12,
  XMM13 = 13,
  XMM14 = 14,
  XMM15 = 15,
  kNumberOfXmmRegisters = 16,
  kNoXmmRegister = -1,
};

template <typename T>
struct Option
{
};

template <typename T>
struct Vec
{
};

struct f32x4
{
  float v1, v2, v3, v4;
};

struct Value
{
  enum class Tag
  {
    Ptr,
    Float,
    Double,
    Int,
    F4,
  };

  struct Ptr_Body
  {
    const uint8_t *_0;
  };

  struct Float_Body
  {
    float _0;
  };

  struct Double_Body
  {
    double _0;
  };

  struct Int_Body
  {
    int32_t _0;
  };

  struct F4_Body
  {
    f32x4 _0;
  };

  Tag tag;
  union {
    Ptr_Body ptr;
    Float_Body float_;
    Double_Body double_;
    Int_Body int_;
    F4_Body f4;
  };
};

struct Entry
{
  int32_t disp;
  Value value;
};

struct DSeg
{
  Vec<Entry> entries;
  int32_t size;
};

struct ForwardJump
{
  uintptr_t at;
  uintptr_t to;
};

struct Assembler
{
  Vec<uint8_t> data;
  DSeg dseg;
  Vec<ForwardJump> jumps;
  Vec<Option<uintptr_t>> labels;
};

struct Reg
{
  enum class Tag
  {
    Gpr,
    Float
  };

  struct Gpr_Body
  {
    Register _0;
  };
  struct Float_Body
  {
    XMMRegister _0;
  };
  Tag tag;
  union {
    Gpr_Body gpr;
    Float_Body fpr;
  };
};

struct Mem
{
  enum class Tag
  {
    Local,
    Base,
    Index,
    Offset,
  };

  struct Local_Body
  {
    int32_t _0;
  };

  struct Base_Body
  {
    Register _0;
    int32_t _1;
  };

  struct Index_Body
  {
    Register _0;
    Register _1;
    int32_t _2;
    int32_t _3;
  };

  struct Offset_Body
  {
    Register _0;
    int32_t _1;
    int32_t _2;
  };

  Tag tag;
  union {
    Local_Body local;
    Base_Body base;
    Index_Body index;
    Offset_Body offset;
  };
};

using Label = uintptr_t;

struct Memory
{
  const uint8_t *start;
  const uint8_t *end;
  const uint8_t *pointer;
  uintptr_t size;
};

extern "C"
{

  void asm_load_int(Assembler *buf, MachineMode mode, Register dest, long imm);
  void asm_load_float(Assembler *buf, MachineMode mode, XMMRegister dest, double imm);

  void addps(Assembler *buf, XMMRegister dest, XMMRegister src);

  void addsd(Assembler *buf, XMMRegister dest, XMMRegister src);

  void addss(Assembler *buf, XMMRegister dest, XMMRegister src);

  void cmov(Assembler *buf,
            uint8_t x64,
            Register dest,
            Register src,
            CondCode cond);

  void cvtps2dq(Assembler *buf, XMMRegister dst, XMMRegister src);

  void cvtps2dq_mem(Assembler *buf, XMMRegister dst, Mem src);

  void cvtsd2ss(Assembler *buf, XMMRegister dest, XMMRegister src);

  void cvtsi2sd(Assembler *buf, XMMRegister dest, uint8_t x64, Register src);

  void cvtsi2ss(Assembler *buf, XMMRegister dest, uint8_t x64, Register src);

  void cvtss2sd(Assembler *buf, XMMRegister dest, XMMRegister src);

  void cvttsd2si(Assembler *buf, uint8_t x64, Register dest, XMMRegister src);

  void cvttss2si(Assembler *buf, uint8_t x64, Register dest, XMMRegister src);

  void divps(Assembler *buf, XMMRegister dest, XMMRegister src);

  void divsd(Assembler *buf, XMMRegister dest, XMMRegister src);

  void divss(Assembler *buf, XMMRegister dest, XMMRegister src);

  void emit(Assembler *buf, uint8_t val);

  void emit32(Assembler *buf, uint32_t val);

  void emit64(Assembler *buf, uint64_t val);

  void emit_add_reg_reg(Assembler *buf,
                        uint8_t x64,
                        Register src,
                        Register dest);

  void emit_addq_imm_reg(Assembler *buf, int32_t imm, Register reg);

  void emit_and_reg_reg(Assembler *buf,
                        uint8_t x64,
                        Register src,
                        Register dest);

  void emit_andb_imm_reg(Assembler *buf, uint8_t imm, Register dest);

  void emit_andq_imm_reg(Assembler *buf, int32_t imm, Register reg);

  void emit_callq_reg(Assembler *buf, Register dest);

  void emit_cdq(Assembler *buf);

  void emit_cmp_imm_reg(Assembler *buf,
                        MachineMode mode,
                        int32_t imm,
                        Register reg);

  void emit_cmp_mem_imm(Assembler *buf,
                        MachineMode mode,
                        Register base,
                        int32_t disp,
                        int32_t imm);

  void emit_cmp_mem_reg(Assembler *buf,
                        MachineMode mode,
                        Register base,
                        int32_t disp,
                        Register dest);

  void emit_cmp_memindex_reg(Assembler *buf,
                             MachineMode mode,
                             Register base,
                             Register index,
                             int32_t scale,
                             int32_t disp,
                             Register dest);

  void emit_cmp_reg_reg(Assembler *buf,
                        uint8_t x64,
                        Register src,
                        Register dest);

  void emit_cmpb_imm_reg(Assembler *buf, uint8_t imm, Register dest);

  void emit_cqo(Assembler *buf);

  void emit_idiv_reg_reg(Assembler *buf, uint8_t x64, Register reg);

  void emit_imul_reg_reg(Assembler *buf,
                         uint8_t x64,
                         Register src,
                         Register dest);

  void emit_jcc(Assembler *buf, CondCode cond, Label lbl);

  void emit_jmp(Assembler *buf, Label lbl);

  void emit_jmp_reg(Assembler *buf, Register reg);

  void emit_mem(Assembler *buf, Register dest, const Mem *src);

  void emit_modrm(Assembler *buf, uint8_t mode, uint8_t reg, uint8_t rm);

  void emit_mov_memindex_reg(Assembler *buf,
                             MachineMode mode,
                             Register base,
                             Register index,
                             int32_t scale,
                             int32_t disp,
                             Register dest);

  void emit_mov_reg_memindex(Assembler *buf,
                             MachineMode mode,
                             Register src,
                             Register base,
                             Register index,
                             int32_t scale,
                             int32_t disp);

  void emit_mov_reg_reg(Assembler *buf,
                        uint8_t x64,
                        Register src,
                        Register dest);

  void emit_movb_imm_memq(Assembler *buf,
                          uint8_t imm,
                          Register dest,
                          int32_t disp);

  void emit_movb_imm_memscaleq(Assembler *buf,
                               uint8_t imm,
                               Register base,
                               Register index,
                               uint8_t scale);

  void emit_movb_memq_reg(Assembler *buf,
                          Register src,
                          int32_t disp,
                          Register dest);

  void emit_movb_reg_memq(Assembler *buf,
                          Register src,
                          Register dest,
                          int32_t disp);

  void emit_movb_reg_reg(Assembler *buf, Register src, Register dest);

  void emit_movl_ar(Assembler *buf,
                    Register base,
                    Register index,
                    uint8_t scale,
                    Register dest);

  void emit_movl_imm_reg(Assembler *buf, int32_t imm, Register reg);

  void emit_movl_memq_reg(Assembler *buf,
                          Register src,
                          int32_t disp,
                          Register dest);

  void emit_movl_ra(Assembler *buf,
                    Register src,
                    Register base,
                    Register index,
                    uint8_t scale);

  void emit_movl_reg_memq(Assembler *buf,
                          Register src,
                          Register dest,
                          int32_t disp);

  void emit_movq_ar(Assembler *buf,
                    Register base,
                    Register index,
                    uint8_t scale,
                    Register dest);

  void emit_movq_imm64_reg(Assembler *buf, int64_t imm, Register reg);

  void emit_movq_imm_reg(Assembler *buf, int32_t imm, Register reg);

  void emit_movq_memq_reg(Assembler *buf,
                          Register src,
                          int32_t disp,
                          Register dest);

  void emit_movq_ra(Assembler *buf,
                    Register src,
                    Register base,
                    Register index,
                    uint8_t scale);

  void emit_movq_reg_memq(Assembler *buf,
                          Register src,
                          Register dest,
                          int32_t disp);

  void emit_movsx(Assembler *buf, Register src, Register dest);

  void emit_movzbl_memq_reg(Assembler *buf,
                            Register src,
                            int32_t disp,
                            Register dest);

  void emit_movzbl_reg_reg(Assembler *buf, Register src, Register dest);

  void emit_movzx_memindex_byte_reg(Assembler *buf,
                                    uint8_t x64,
                                    Register base,
                                    Register index,
                                    int32_t disp,
                                    Register dest);

  void emit_neg_reg(Assembler *buf, uint8_t x64, Register reg);

  void emit_nop(Assembler *buf);

  void emit_not_reg(Assembler *buf, uint8_t x64, Register reg);

  void emit_not_reg_byte(Assembler *buf, Register reg);

  void emit_op(Assembler *buf, uint8_t opcode);

  void emit_or_reg_reg(Assembler *buf,
                       uint8_t x64,
                       Register src,
                       Register dest);

  void emit_popq_reg(Assembler *buf, Register reg);

  void emit_pushq_reg(Assembler *buf, Register reg);

  void emit_retq(Assembler *buf);

  void emit_rex(Assembler *buf, uint8_t w, uint8_t r, uint8_t x, uint8_t b);

  void emit_rex_mem(Assembler *buf, uint8_t x64, Register dest, const Mem *src);

  void emit_setb_reg(Assembler *buf, CondCode op, Register reg);

  void emit_setb_reg_parity(Assembler *buf, Register reg, bool parity);

  void emit_shl_reg_cl(Assembler *buf, uint8_t x64, Register dest);

  void emit_shll_reg(Assembler *buf, uint8_t imm, Register dest);

  void emit_shlq_reg(Assembler *buf, uint8_t imm, Register dest);

  void emit_shr_reg_cl(Assembler *buf, uint8_t x64, Register dest);

  void emit_sib(Assembler *buf, uint8_t scale, uint8_t index, uint8_t base);

  void emit_sub_imm_mem(Assembler *buf,
                        MachineMode mode,
                        Register base,
                        uint8_t imm);

  void emit_sub_reg_reg(Assembler *buf,
                        uint8_t x64,
                        Register src,
                        Register dest);

  void emit_subq_imm_reg(Assembler *buf, int32_t imm, Register reg);

  void emit_testl_reg_reg(Assembler *buf, Register op1, Register op2);

  void emit_testq_reg_reg(Assembler *buf, Register op1, Register op2);

  void emit_xor_reg_reg(Assembler *buf,
                        uint8_t x64,
                        Register src,
                        Register dest);

  void emit_xorb_imm_reg(Assembler *buf, uint8_t imm, Register dest);

  bool fits_i32(int64_t n);

  bool fits_i8(int32_t imm);

  Memory get_executable_memory(const Assembler *buf);

  void lea(Assembler *buf, Register dest, Mem src);

  Reg reg_gpr(Register reg);
  Reg reg_fpr(XMMRegister reg);
  Mem mem_base(Register reg, int32_t off);

  Mem mem_index(Register reg, Register reg2, int32_t v1, int32_t v2);

  Mem mem_local(int32_t off);

  Mem mem_offset(Register reg, int32_t v1, int32_t v2);

  void movaps(Assembler *buf, XMMRegister dest, XMMRegister src);

  void movaps_load(Assembler *buf, XMMRegister dest, Mem src);

  void movaps_store(Assembler *buf, Mem dest, XMMRegister src);

  void movd_freg_reg(Assembler *buf, XMMRegister dest, Register src);

  void movd_reg_freg(Assembler *buf, Register dest, XMMRegister src);

  void movlps(Assembler *buf, XMMRegister dest, XMMRegister src);

  void movq_freg_reg(Assembler *buf, XMMRegister dest, Register src);

  void movq_reg_freg(Assembler *buf, Register dest, XMMRegister src);

  void movsd(Assembler *buf, XMMRegister dest, XMMRegister src);

  void movsd_load(Assembler *buf, XMMRegister dest, Mem mem);

  void movsd_store(Assembler *buf, Mem mem, XMMRegister src);

  void movss(Assembler *buf, XMMRegister dest, XMMRegister src);

  void movss_load(Assembler *buf, XMMRegister dest, Mem mem);

  void movss_store(Assembler *buf, Mem mem, XMMRegister src);

  void movups(Assembler *buf, XMMRegister dest, XMMRegister src);

  void movups_load(Assembler *buf, XMMRegister dest, Mem src);

  void movups_store(Assembler *buf, Mem dest, XMMRegister src);

  void mulps(Assembler *buf, XMMRegister dest, XMMRegister src);

  void mulsd(Assembler *buf, XMMRegister dest, XMMRegister src);

  void mulss(Assembler *buf, XMMRegister dest, XMMRegister src);

  f32x4 new_f32x4(float v1, float v2, float v3, float v4);

  void pabsb(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pabsb_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pabsd(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pabsd_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pabsw(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pabsw_mem(Assembler *buf, XMMRegister dst, Mem src);

  void packssdw(Assembler *buf, XMMRegister dst, XMMRegister src);

  void packssdw_mem(Assembler *buf, XMMRegister dst, Mem src);

  void packsswb(Assembler *buf, XMMRegister dst, XMMRegister src);

  void packsswb_mem(Assembler *buf, XMMRegister dst, Mem src);

  void packusdw(Assembler *buf, XMMRegister dst, XMMRegister src);

  void packusdw_mem(Assembler *buf, XMMRegister dst, Mem src);

  void packuswb(Assembler *buf, XMMRegister dst, XMMRegister src);

  void packuswb_mem(Assembler *buf, XMMRegister dst, Mem src);

  void paddb(Assembler *buf, XMMRegister dst, XMMRegister src);

  void paddb_mem(Assembler *buf, XMMRegister dst, Mem src);

  void paddd(Assembler *buf, XMMRegister dst, XMMRegister src);

  void paddd_mem(Assembler *buf, XMMRegister dst, Mem src);

  void paddsb(Assembler *buf, XMMRegister dst, XMMRegister src);

  void paddsb_mem(Assembler *buf, XMMRegister dst, Mem src);

  void paddsw(Assembler *buf, XMMRegister dst, XMMRegister src);

  void paddsw_mem(Assembler *buf, XMMRegister dst, Mem src);

  void paddusb(Assembler *buf, XMMRegister dst, XMMRegister src);

  void paddusb_mem(Assembler *buf, XMMRegister dst, Mem src);

  void paddusw(Assembler *buf, XMMRegister dst, XMMRegister src);

  void paddusw_mem(Assembler *buf, XMMRegister dst, Mem src);

  void paddw(Assembler *buf, XMMRegister dst, XMMRegister src);

  void paddw_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pand(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pand_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pcmpeqb(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pcmpeqb_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pcmpeqd(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pcmpeqd_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pcmpeqw(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pcmpeqw_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pcmpgtb(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pcmpgtb_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pcmpgtd(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pcmpgtd_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pcmpgtw(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pcmpgtw_mem(Assembler *buf, XMMRegister dst, Mem src);

  void phaddd(Assembler *buf, XMMRegister dst, XMMRegister src);

  void phaddd_mem(Assembler *buf, XMMRegister dst, Mem src);

  void phaddw(Assembler *buf, XMMRegister dst, XMMRegister src);

  void phaddw_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pmaxsb(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pmaxsb_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pmaxsd(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pmaxsd_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pmaxsw(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pmaxsw_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pmaxub(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pmaxub_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pmaxud(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pmaxud_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pmaxuw(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pmaxuw_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pminsb(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pminsb_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pminsd(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pminsd_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pminsw(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pminsw_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pminub(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pminub_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pminud(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pminud_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pminuw(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pminuw_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pmovsxbw(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pmovsxbw_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pmovsxwd(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pmovsxwd_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pmovzxbw(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pmovzxbw_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pmovzxwd(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pmovzxwd_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pmulld(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pmulld_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pmullw(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pmullw_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pmuludq(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pmuludq_mem(Assembler *buf, XMMRegister dst, Mem src);

  void por(Assembler *buf, XMMRegister dst, XMMRegister src);

  void por_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pshufb(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pshufb_mem(Assembler *buf, XMMRegister dst, Mem src);

  void psignb(Assembler *buf, XMMRegister dst, XMMRegister src);

  void psignb_mem(Assembler *buf, XMMRegister dst, Mem src);

  void psignd(Assembler *buf, XMMRegister dst, XMMRegister src);

  void psignd_mem(Assembler *buf, XMMRegister dst, Mem src);

  void psignw(Assembler *buf, XMMRegister dst, XMMRegister src);

  void psignw_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pslld(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pslld_mem(Assembler *buf, XMMRegister dst, Mem src);

  void psllw(Assembler *buf, XMMRegister dst, XMMRegister src);

  void psllw_mem(Assembler *buf, XMMRegister dst, Mem src);

  void psrad(Assembler *buf, XMMRegister dst, XMMRegister src);

  void psrad_mem(Assembler *buf, XMMRegister dst, Mem src);

  void psraw(Assembler *buf, XMMRegister dst, XMMRegister src);

  void psraw_mem(Assembler *buf, XMMRegister dst, Mem src);

  void psrld(Assembler *buf, XMMRegister dst, XMMRegister src);

  void psrld_mem(Assembler *buf, XMMRegister dst, Mem src);

  void psrlw(Assembler *buf, XMMRegister dst, XMMRegister src);

  void psrlw_mem(Assembler *buf, XMMRegister dst, Mem src);

  void psubb(Assembler *buf, XMMRegister dst, XMMRegister src);

  void psubb_mem(Assembler *buf, XMMRegister dst, Mem src);

  void psubd(Assembler *buf, XMMRegister dst, XMMRegister src);

  void psubd_mem(Assembler *buf, XMMRegister dst, Mem src);

  void psubsb(Assembler *buf, XMMRegister dst, XMMRegister src);

  void psubsb_mem(Assembler *buf, XMMRegister dst, Mem src);

  void psubsw(Assembler *buf, XMMRegister dst, XMMRegister src);

  void psubsw_mem(Assembler *buf, XMMRegister dst, Mem src);

  void psubusb(Assembler *buf, XMMRegister dst, XMMRegister src);

  void psubusb_mem(Assembler *buf, XMMRegister dst, Mem src);

  void psubusw(Assembler *buf, XMMRegister dst, XMMRegister src);

  void psubusw_mem(Assembler *buf, XMMRegister dst, Mem src);

  void psubw(Assembler *buf, XMMRegister dst, XMMRegister src);

  void psubw_mem(Assembler *buf, XMMRegister dst, Mem src);

  void ptest(Assembler *buf, XMMRegister dst, XMMRegister src);

  void punpckhbw(Assembler *buf, XMMRegister dst, XMMRegister src);

  void punpckhbw_mem(Assembler *buf, XMMRegister dst, Mem src);

  void punpckhdq(Assembler *buf, XMMRegister dst, XMMRegister src);

  void punpckhdq_mem(Assembler *buf, XMMRegister dst, Mem src);

  void punpckhqdq(Assembler *buf, XMMRegister dst, XMMRegister src);

  void punpckhqdq_mem(Assembler *buf, XMMRegister dst, Mem src);

  void punpckhwd(Assembler *buf, XMMRegister dst, XMMRegister src);

  void punpckhwd_mem(Assembler *buf, XMMRegister dst, Mem src);

  void punpcklbw(Assembler *buf, XMMRegister dst, XMMRegister src);

  void punpcklbw_mem(Assembler *buf, XMMRegister dst, Mem src);

  void punpckldq(Assembler *buf, XMMRegister dst, XMMRegister src);

  void punpckldq_mem(Assembler *buf, XMMRegister dst, Mem src);

  void punpcklqdq(Assembler *buf, XMMRegister dst, XMMRegister src);

  void punpcklqdq_mem(Assembler *buf, XMMRegister dst, Mem src);

  void punpcklwd(Assembler *buf, XMMRegister dst, XMMRegister src);

  void punpcklwd_mem(Assembler *buf, XMMRegister dst, Mem src);

  void pxor(Assembler *buf, XMMRegister dst, XMMRegister src);

  void pxor_mem(Assembler *buf, XMMRegister dst, Mem src);

  void sqrtps(Assembler *buf, XMMRegister dest, XMMRegister src);

  void sqrtsd(Assembler *buf, XMMRegister dest, XMMRegister src);

  void sqrtss(Assembler *buf, XMMRegister dest, XMMRegister src);

  void sse_float_freg_freg(Assembler *buf,
                           bool dbl,
                           uint8_t op,
                           XMMRegister dest,
                           XMMRegister src);

  void sse_float_freg_mem(Assembler *buf,
                          bool dbl,
                          uint8_t op,
                          XMMRegister dest,
                          Mem src);

  void sse_float_freg_mem_66(Assembler *buf,
                             bool dbl,
                             uint8_t op,
                             XMMRegister dest,
                             Mem src);

  void sse_float_freg_reg(Assembler *buf,
                          bool dbl,
                          uint8_t op,
                          XMMRegister dest,
                          uint8_t x64,
                          Register src);

  void sse_float_reg_freg(Assembler *buf,
                          bool dbl,
                          uint8_t op,
                          uint8_t x64,
                          Register dest,
                          XMMRegister src);

  void sse_packed_freg_freg(Assembler *buf,
                            uint8_t op,
                            XMMRegister dest,
                            XMMRegister src);

  void sse_packed_freg_mem(Assembler *buf,
                           uint8_t op,
                           XMMRegister dest,
                           Mem src);

  void subps(Assembler *buf, XMMRegister dest, XMMRegister src);

  void subsd(Assembler *buf, XMMRegister dest, XMMRegister src);

  void subss(Assembler *buf, XMMRegister dest, XMMRegister src);

  void testl_reg_mem(Assembler *buf, Register dest, Mem src);

  void ucomisd(Assembler *buf, XMMRegister dest, XMMRegister src);

  void ucomiss(Assembler *buf, XMMRegister dest, XMMRegister src);

  void vcvtps2dq(Assembler *buf,
                 XMMRegister dst,
                 XMMRegister src1,
                 XMMRegister src2);

  void vmovaps(Assembler *buf, XMMRegister dst, XMMRegister src);

  void vmovaps_mem(Assembler *buf, XMMRegister dst, Mem src);

  void vmovd_freg_mem(Assembler *buf, XMMRegister dst, Mem src);

  void vmovd_reg_freg(Assembler *buf, Register dst, XMMRegister src);

  void vmovq_freg_mem(Assembler *buf, XMMRegister dst, Mem src);

  void vmovq_reg_freg(Assembler *buf, Register dst, XMMRegister src);

  void vpackssdw(Assembler *buf,
                 XMMRegister dst,
                 XMMRegister src1,
                 XMMRegister src2);

  void vpackssdw_mem(Assembler *buf,
                     XMMRegister dst,
                     XMMRegister src1,
                     Mem src2);

  void vpacksswb(Assembler *buf,
                 XMMRegister dst,
                 XMMRegister src1,
                 XMMRegister src2);

  void vpacksswb_mem(Assembler *buf,
                     XMMRegister dst,
                     XMMRegister src1,
                     Mem src2);

  void vpackuswb(Assembler *buf,
                 XMMRegister dst,
                 XMMRegister src1,
                 XMMRegister src2);

  void vpackuswb_mem(Assembler *buf,
                     XMMRegister dst,
                     XMMRegister src1,
                     Mem src2);

  void vpaddb(Assembler *buf,
              XMMRegister dst,
              XMMRegister src1,
              XMMRegister src2);

  void vpaddb_mem(Assembler *buf, XMMRegister dst, XMMRegister src1, Mem src2);

  void vpaddd(Assembler *buf,
              XMMRegister dst,
              XMMRegister src1,
              XMMRegister src2);

  void vpaddd_mem(Assembler *buf, XMMRegister dst, XMMRegister src1, Mem src2);

  void vpaddsb(Assembler *buf,
               XMMRegister dst,
               XMMRegister src1,
               XMMRegister src2);

  void vpaddsb_mem(Assembler *buf, XMMRegister dst, XMMRegister src1, Mem src2);

  void vpaddsw(Assembler *buf,
               XMMRegister dst,
               XMMRegister src1,
               XMMRegister src2);

  void vpaddsw_mem(Assembler *buf, XMMRegister dst, XMMRegister src1, Mem src2);

  void vpaddusb(Assembler *buf,
                XMMRegister dst,
                XMMRegister src1,
                XMMRegister src2);

  void vpaddusb_mem(Assembler *buf,
                    XMMRegister dst,
                    XMMRegister src1,
                    Mem src2);

  void vpaddusw(Assembler *buf,
                XMMRegister dst,
                XMMRegister src1,
                XMMRegister src2);

  void vpaddusw_mem(Assembler *buf,
                    XMMRegister dst,
                    XMMRegister src1,
                    Mem src2);

  void vpaddw(Assembler *buf,
              XMMRegister dst,
              XMMRegister src1,
              XMMRegister src2);

  void vpaddw_mem(Assembler *buf, XMMRegister dst, XMMRegister src1, Mem src2);

  void vpand(Assembler *buf,
             XMMRegister dst,
             XMMRegister src1,
             XMMRegister src2);

  void vpand_mem(Assembler *buf, XMMRegister dst, XMMRegister src1, Mem src2);

  void vpcmpeqb(Assembler *buf,
                XMMRegister dst,
                XMMRegister src1,
                XMMRegister src2);

  void vpcmpeqb_mem(Assembler *buf,
                    XMMRegister dst,
                    XMMRegister src1,
                    Mem src2);

  void vpcmpeqd(Assembler *buf,
                XMMRegister dst,
                XMMRegister src1,
                XMMRegister src2);

  void vpcmpeqd_mem(Assembler *buf,
                    XMMRegister dst,
                    XMMRegister src1,
                    Mem src2);

  void vpcmpeqw(Assembler *buf,
                XMMRegister dst,
                XMMRegister src1,
                XMMRegister src2);

  void vpcmpeqw_mem(Assembler *buf,
                    XMMRegister dst,
                    XMMRegister src1,
                    Mem src2);

  void vpcmpgtb(Assembler *buf,
                XMMRegister dst,
                XMMRegister src1,
                XMMRegister src2);

  void vpcmpgtb_mem(Assembler *buf,
                    XMMRegister dst,
                    XMMRegister src1,
                    Mem src2);

  void vpcmpgtd(Assembler *buf,
                XMMRegister dst,
                XMMRegister src1,
                XMMRegister src2);

  void vpcmpgtd_mem(Assembler *buf,
                    XMMRegister dst,
                    XMMRegister src1,
                    Mem src2);

  void vpcmpgtw(Assembler *buf,
                XMMRegister dst,
                XMMRegister src1,
                XMMRegister src2);

  void vpcmpgtw_mem(Assembler *buf,
                    XMMRegister dst,
                    XMMRegister src1,
                    Mem src2);

  void vpmaxsw(Assembler *buf,
               XMMRegister dst,
               XMMRegister src1,
               XMMRegister src2);

  void vpmaxsw_mem(Assembler *buf, XMMRegister dst, XMMRegister src1, Mem src2);

  void vpmaxub(Assembler *buf,
               XMMRegister dst,
               XMMRegister src1,
               XMMRegister src2);

  void vpmaxub_mem(Assembler *buf, XMMRegister dst, XMMRegister src1, Mem src2);

  void vpminsw(Assembler *buf,
               XMMRegister dst,
               XMMRegister src1,
               XMMRegister src2);

  void vpminsw_mem(Assembler *buf, XMMRegister dst, XMMRegister src1, Mem src2);

  void vpminub(Assembler *buf,
               XMMRegister dst,
               XMMRegister src1,
               XMMRegister src2);

  void vpminub_mem(Assembler *buf, XMMRegister dst, XMMRegister src1, Mem src2);

  void vpmullw(Assembler *buf,
               XMMRegister dst,
               XMMRegister src1,
               XMMRegister src2);

  void vpmullw_mem(Assembler *buf, XMMRegister dst, XMMRegister src1, Mem src2);

  void vpmuludq(Assembler *buf,
                XMMRegister dst,
                XMMRegister src1,
                XMMRegister src2);

  void vpmuludq_mem(Assembler *buf,
                    XMMRegister dst,
                    XMMRegister src1,
                    Mem src2);

  void vpor(Assembler *buf,
            XMMRegister dst,
            XMMRegister src1,
            XMMRegister src2);

  void vpor_mem(Assembler *buf, XMMRegister dst, XMMRegister src1, Mem src2);

  void vpslld(Assembler *buf,
              XMMRegister dst,
              XMMRegister src1,
              XMMRegister src2);

  void vpslld_mem(Assembler *buf, XMMRegister dst, XMMRegister src1, Mem src2);

  void vpsllw(Assembler *buf,
              XMMRegister dst,
              XMMRegister src1,
              XMMRegister src2);

  void vpsllw_mem(Assembler *buf, XMMRegister dst, XMMRegister src1, Mem src2);

  void vpsrad(Assembler *buf,
              XMMRegister dst,
              XMMRegister src1,
              XMMRegister src2);

  void vpsrad_mem(Assembler *buf, XMMRegister dst, XMMRegister src1, Mem src2);

  void vpsraw(Assembler *buf,
              XMMRegister dst,
              XMMRegister src1,
              XMMRegister src2);

  void vpsraw_mem(Assembler *buf, XMMRegister dst, XMMRegister src1, Mem src2);

  void vpsrld(Assembler *buf,
              XMMRegister dst,
              XMMRegister src1,
              XMMRegister src2);

  void vpsrld_mem(Assembler *buf, XMMRegister dst, XMMRegister src1, Mem src2);

  void vpsrlw(Assembler *buf,
              XMMRegister dst,
              XMMRegister src1,
              XMMRegister src2);

  void vpsrlw_mem(Assembler *buf, XMMRegister dst, XMMRegister src1, Mem src2);

  void vpsubb(Assembler *buf,
              XMMRegister dst,
              XMMRegister src1,
              XMMRegister src2);

  void vpsubb_mem(Assembler *buf, XMMRegister dst, XMMRegister src1, Mem src2);

  void vpsubd(Assembler *buf,
              XMMRegister dst,
              XMMRegister src1,
              XMMRegister src2);

  void vpsubd_mem(Assembler *buf, XMMRegister dst, XMMRegister src1, Mem src2);

  void vpsubsb(Assembler *buf,
               XMMRegister dst,
               XMMRegister src1,
               XMMRegister src2);

  void vpsubsb_mem(Assembler *buf, XMMRegister dst, XMMRegister src1, Mem src2);

  void vpsubsw(Assembler *buf,
               XMMRegister dst,
               XMMRegister src1,
               XMMRegister src2);

  void vpsubsw_mem(Assembler *buf, XMMRegister dst, XMMRegister src1, Mem src2);

  void vpsubusb(Assembler *buf,
                XMMRegister dst,
                XMMRegister src1,
                XMMRegister src2);

  void vpsubusb_mem(Assembler *buf,
                    XMMRegister dst,
                    XMMRegister src1,
                    Mem src2);

  void vpsubusw(Assembler *buf,
                XMMRegister dst,
                XMMRegister src1,
                XMMRegister src2);

  void vpsubusw_mem(Assembler *buf,
                    XMMRegister dst,
                    XMMRegister src1,
                    Mem src2);

  void vpsubw(Assembler *buf,
              XMMRegister dst,
              XMMRegister src1,
              XMMRegister src2);

  void vpsubw_mem(Assembler *buf, XMMRegister dst, XMMRegister src1, Mem src2);

  void vpunpckhbw(Assembler *buf,
                  XMMRegister dst,
                  XMMRegister src1,
                  XMMRegister src2);

  void vpunpckhbw_mem(Assembler *buf,
                      XMMRegister dst,
                      XMMRegister src1,
                      Mem src2);

  void vpunpckhdq(Assembler *buf,
                  XMMRegister dst,
                  XMMRegister src1,
                  XMMRegister src2);

  void vpunpckhdq_mem(Assembler *buf,
                      XMMRegister dst,
                      XMMRegister src1,
                      Mem src2);

  void vpunpckhqdq(Assembler *buf,
                   XMMRegister dst,
                   XMMRegister src1,
                   XMMRegister src2);

  void vpunpckhqdq_mem(Assembler *buf,
                       XMMRegister dst,
                       XMMRegister src1,
                       Mem src2);

  void vpunpckhwd(Assembler *buf,
                  XMMRegister dst,
                  XMMRegister src1,
                  XMMRegister src2);

  void vpunpckhwd_mem(Assembler *buf,
                      XMMRegister dst,
                      XMMRegister src1,
                      Mem src2);

  void vpunpcklbw(Assembler *buf,
                  XMMRegister dst,
                  XMMRegister src1,
                  XMMRegister src2);

  void vpunpcklbw_mem(Assembler *buf,
                      XMMRegister dst,
                      XMMRegister src1,
                      Mem src2);

  void vpunpckldq(Assembler *buf,
                  XMMRegister dst,
                  XMMRegister src1,
                  XMMRegister src2);

  void vpunpckldq_mem(Assembler *buf,
                      XMMRegister dst,
                      XMMRegister src1,
                      Mem src2);

  void vpunpcklqdq(Assembler *buf,
                   XMMRegister dst,
                   XMMRegister src1,
                   XMMRegister src2);

  void vpunpcklqdq_mem(Assembler *buf,
                       XMMRegister dst,
                       XMMRegister src1,
                       Mem src2);

  void vpunpcklwd(Assembler *buf,
                  XMMRegister dst,
                  XMMRegister src1,
                  XMMRegister src2);

  void vpunpcklwd_mem(Assembler *buf,
                      XMMRegister dst,
                      XMMRegister src1,
                      Mem src2);

  void vpxor(Assembler *buf,
             XMMRegister dst,
             XMMRegister src1,
             XMMRegister src2);

  void vpxor_mem(Assembler *buf, XMMRegister dst, XMMRegister src1, Mem src2);

  void xorpd(Assembler *buf, XMMRegister dest, Mem src);

  void xorps(Assembler *buf, XMMRegister dest, Mem src);

} // extern "C"

} // namespace jazz_jit

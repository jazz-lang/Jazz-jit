///! This file contains functions that emit's vector instructions
///! WARNING: Still not implemented
///!

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(i32)]
pub enum VectorLength {
    kL128 = 0x0,
    kL256 = 0x4,
}

pub const kLIG: VectorLength = VectorLength::kL128;
pub const kLZ: VectorLength = VectorLength::kL256;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(i32)]
pub enum VexW {
    W0 = 0x0,
    W1 = 0x80,
}

pub const WIG: VexW = VexW::W0;
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(i32)]
pub enum LeadingOpcode {
    k0F = 0x1,
    k0F38 = 0x2,
    k0F3A = 0x3,
}
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(i32)]
pub enum SIMDPrefix {
    None = 0x0,
    k66 = 0x1,
    kF3 = 0x2,
    KF2 = 0x3,
}

use crate::assembler::*;
use crate::assembler_x64::*;
use crate::constants_x64::*;

fn emit_vex3_byte1_1(asm: &mut Assembler, reg: XMMRegister, rm: XMMRegister, m: LeadingOpcode) {
    let rxb = !((reg.high_bit() << 2) | rm.high_bit()) << 5;
    asm.emit(rxb | m as u8);
}

use std::mem;

fn emit_vex3_byte1_2(asm: &mut Assembler, reg: XMMRegister, rm: Mem, m: LeadingOpcode) {
    let rxb = emit_rex_memv(asm, 1, unsafe { mem::transmute(reg) }, &rm);
    asm.emit(rxb | m as u8);
}

fn emit_vex2_byte1(
    asm: &mut Assembler,
    reg: XMMRegister,
    v: XMMRegister,
    l: VectorLength,
    pp: SIMDPrefix,
) {
    let rv = !((reg.high_bit() << 4) | v as u8) << 3;
    asm.emit(rv | l as u8 | pp as u8);
}

fn emit_vex3_byte2(asm: &mut Assembler, w: VexW, v: XMMRegister, l: VectorLength, pp: SIMDPrefix) {
    let v = v as u8;
    asm.emit(w as u8 | ((!v & 0xf) << 3) | l as u8 | pp as u8);
}

fn emit_vex_prefixf(
    asm: &mut Assembler,
    reg: XMMRegister,
    vreg: XMMRegister,
    rm: XMMRegister,
    l: VectorLength,
    pp: SIMDPrefix,
    mm: LeadingOpcode,
    w: VexW,
) {
    if rm.high_bit() != 0 || mm != LeadingOpcode::k0F || w != VexW::W0 {
        emit_vex3_byte0(asm);
        emit_vex3_byte1_1(asm, reg, rm, mm);
        emit_vex3_byte2(asm, w, vreg, l, pp);
    } else {
        emit_vex2_byte0(asm);
        emit_vex2_byte1(asm, reg, vreg, l, pp);
    }
}

fn emit_vex_prefixr(
    asm: &mut Assembler,
    reg: Register,
    vreg: Register,
    rm: Register,
    l: VectorLength,
    pp: SIMDPrefix,
    mm: LeadingOpcode,
    w: VexW,
) {
    unsafe {
        emit_vex_prefixf(
            asm,
            mem::transmute(reg),
            mem::transmute(vreg),
            mem::transmute(rm),
            l,
            pp,
            mm,
            w,
        );
    }
}

fn emit_vex_prefixfm(
    asm: &mut Assembler,
    reg: XMMRegister,
    vreg: XMMRegister,
    rm: Mem,
    l: VectorLength,
    pp: SIMDPrefix,
    mm: LeadingOpcode,
    w: VexW,
) {
    let rex = emit_rex_memv(asm, 1, unsafe { mem::transmute(reg) }, &rm);
    if rex != 0 || mm != LeadingOpcode::k0F || w != VexW::W0 {
        emit_vex3_byte0(asm);
        emit_vex3_byte1_2(asm, reg, rm, mm);
        emit_vex3_byte2(asm, w, vreg, l, pp);
    } else {
        emit_vex2_byte0(asm);
        emit_vex2_byte1(asm, reg, vreg, l, pp);
    }
}

fn emit_vex_prefixrm(
    asm: &mut Assembler,
    reg: Register,
    vreg: Register,
    rm: Mem,
    l: VectorLength,
    pp: SIMDPrefix,
    mm: LeadingOpcode,
    w: VexW,
) {
    emit_vex_prefixfm(
        asm,
        unsafe { mem::transmute(reg) },
        unsafe { mem::transmute(vreg) },
        rm,
        l,
        pp,
        mm,
        w,
    );
}

fn emit_vex2_byte0(asm: &mut Assembler) {
    asm.emit(0xc5);
}

fn emit_vex3_byte0(asm: &mut Assembler) {
    asm.emit(0xc4);
}

fn emit_rex_memv(buf: &mut Assembler, x64: u8, dest: Register, src: &Mem) -> u8 {
    assert!(x64 == 0 || x64 == 1);

    let (base_msb, index_msb) = match src {
        &Mem::Local(_) => (RBP.msb(), 0),
        &Mem::Base(base, _) => {
            let base_msb = if base == RIP { 0 } else { base.msb() };

            (base_msb, 0)
        }

        &Mem::Index(base, index, _, _) => (base.msb(), index.msb()),
        &Mem::Offset(index, _, _) => (0, index.msb()),
    };

    if dest.msb() != 0 || index_msb != 0 || base_msb != 0 || x64 != 0 {
        return emit_rexv(buf, x64, dest.msb(), index_msb, base_msb);
    }
    return 0;
}
fn emit_rexv(buf: &mut Assembler, w: u8, r: u8, x: u8, b: u8) -> u8 {
    assert!(w == 0 || w == 1);
    assert!(r == 0 || r == 1);
    assert!(x == 0 || x == 1);
    assert!(b == 0 || b == 1);

    (0x4 << 4 | w << 3 | r << 2 | x << 1 | b)
}

fn emit_sse_ff(asm: &mut Assembler, dst: XMMRegister, src: XMMRegister) {
    asm.emit(0xc0 | (dst.low_bit() << 3) | src.low_bit());
}
fn emit_sse_fr(asm: &mut Assembler, dst: XMMRegister, src: Register) {
    asm.emit(0xc0 | (dst.low_bit() << 3) | src.low_bit());
}

fn emit_sse_mem_f(asm: &mut Assembler, dst: XMMRegister, src: Mem) {
    emit_mem(asm, unsafe { mem::transmute(dst) }, &src);
}

pub fn vinstr(
    asm: &mut Assembler,
    op: u8,
    dst: XMMRegister,
    src1: XMMRegister,
    src2: XMMRegister,
    pp: SIMDPrefix,
    m: LeadingOpcode,
    w: VexW,
) {
    emit_vex_prefixf(asm, dst, src1, src2, VectorLength::kL128, pp, m, w);
    asm.emit(op);
    emit_sse_ff(asm, dst, src2);
}

pub fn vinstrm(
    asm: &mut Assembler,
    op: u8,
    dst: XMMRegister,
    src1: XMMRegister,
    src2: Mem,
    pp: SIMDPrefix,
    m: LeadingOpcode,
    w: VexW,
) {
    emit_vex_prefixfm(asm, dst, src1, src2, VectorLength::kL128, pp, m, w);
    asm.emit(op);
    emit_sse_mem_f(asm, dst, src2);
}

pub fn vps(asm: &mut Assembler, op: u8, dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {
    emit_vex_prefixf(
        asm,
        dst,
        src1,
        src2,
        VectorLength::kL128,
        SIMDPrefix::None,
        LeadingOpcode::k0F,
        WIG,
    );
    asm.emit(op);
    emit_sse_ff(asm, dst, src2);
}

pub fn vpsm(asm: &mut Assembler, op: u8, dst: XMMRegister, src1: XMMRegister, src2: Mem) {
    emit_vex_prefixfm(
        asm,
        dst,
        src1,
        src2,
        VectorLength::kL128,
        SIMDPrefix::None,
        LeadingOpcode::k0F,
        WIG,
    );
    asm.emit(op);
    emit_sse_mem_f(asm, dst, src2);
}

pub fn vpd(asm: &mut Assembler, op: u8, dst: XMMRegister, src1: XMMRegister, src2: XMMRegister) {
    emit_vex_prefixf(
        asm,
        dst,
        src1,
        src2,
        VectorLength::kL128,
        SIMDPrefix::k66,
        LeadingOpcode::k0F,
        WIG,
    );
    asm.emit(op);
    emit_sse_ff(asm, dst, src2);
}

pub fn vpdm(asm: &mut Assembler, op: u8, dst: XMMRegister, src1: XMMRegister, src2: Mem) {
    emit_vex_prefixfm(
        asm,
        dst,
        src1,
        src2,
        VectorLength::kL128,
        SIMDPrefix::k66,
        LeadingOpcode::k0F,
        WIG,
    );
    asm.emit(op);
    emit_sse_mem_f(asm, dst, src2);
}

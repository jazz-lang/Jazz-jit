extern crate jazz_jit;

use jazz_jit::assembler::Assembler;
use jazz_jit::assembler_x64::*;
use jazz_jit::constants_x64::*;
use jazz_jit::vector_extension_x64::*;
extern crate capstone;

use capstone::prelude::*;

fn main() {
    let mut asm = Assembler::new();
    vmovd_freg_reg(&mut asm, XMM1, RAX);

    let data = asm.data();

    let mut cs = Capstone::new()
        .x86()
        .mode(arch::x86::ArchMode::Mode64)
        .syntax(arch::x86::ArchSyntax::Intel)
        .detail(true)
        .build()
        .unwrap();

    let ins = cs.disasm_all(data, 0x0);
    for ins in ins.unwrap().iter() {
        println!("{}", ins);
    }
}

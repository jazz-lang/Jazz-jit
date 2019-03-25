extern crate jazz_jit;

use jazz_jit::assembler::Assembler;
use jazz_jit::assembler_x64::*;
use jazz_jit::avx::*;
use jazz_jit::constants_x64::*;
use jazz_jit::get_executable_memory;
extern crate capstone;

use capstone::prelude::*;

fn main() {
    let mut asm = Assembler::new();
    vaddpd(&mut asm, XMM0, XMM1, XMM2);
    emit_retq(&mut asm);
    let data = asm.data();
    for byte in data.iter() {
        print!("0x{:x} ", byte);
    }
    println!("");
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

#![allow(non_camel_case_types)]
#![allow(non_upper_case_globals)]
#![allow(unused_imports)]
#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_macros)]

extern crate jazz_jit;

use jazz_jit::assembler::Assembler;
use jazz_jit::assembler::Mem;
use jazz_jit::assembler_x64::*;
use jazz_jit::generic::*;
use jazz_jit::avx::*;
use jazz_jit::constants_x64::*;
use jazz_jit::dseg::f32x4;
use jazz_jit::get_executable_memory;
extern crate capstone;

use capstone::prelude::*;

fn main() {
    let mut asm = Assembler::new();
    asm.mov(false,RDI,RAX);
    asm.mov(false,2,RAX);
    let data = asm.data();
    for byte in data.iter() {
        print!("0x{:x} ", byte);
    }
    println!("");
    let cs = Capstone::new().x86()
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

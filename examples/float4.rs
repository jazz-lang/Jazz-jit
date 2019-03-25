#![feature(repr_simd)]
extern crate jazz_jit;

use std::arch::x86_64::*;

use capstone::prelude::*;
use jazz_jit::assembler::Assembler;
use jazz_jit::assembler_x64::*;
use jazz_jit::avx::*;
use jazz_jit::constants_x64::*;
use jazz_jit::dseg::f32x4;
use jazz_jit::get_executable_memory;
use jazz_jit::MachineMode::Float32;
fn main() {
    let mut asm = Assembler::new();
    unsafe {
        asm.load_float4_const(XMM0, f32x4(1.0, 2.0, 3.0, 4.0));
        emit_retq(&mut asm);

        let data = asm.data();
        for byte in data.iter() {
            print!("0x{:x} ", byte);
        }
        println!("");
        let mut cs = Capstone::new().x86()
                                    .mode(arch::x86::ArchMode::Mode64)
                                    .syntax(arch::x86::ArchSyntax::Intel)
                                    .detail(true)
                                    .build()
                                    .unwrap();

        let ins = cs.disasm_all(data, 0x0);
        for ins in ins.unwrap().iter() {
            println!("{}", ins);
        }

        let memory = get_executable_memory(&asm);

        let f: fn() -> __m128 = std::mem::transmute(memory.start());
        println!("{:?}", f());
    }
}

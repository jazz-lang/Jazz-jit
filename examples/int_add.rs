extern crate capstone;
extern crate jazz_jit;

use capstone::prelude::*;

use jazz_jit::assembler::Assembler;
use jazz_jit::assembler_x64::*;
use jazz_jit::constants_x64::*;
use jazz_jit::{get_executable_memory, Memory};

fn main() {
    let mut asm = Assembler::new();

    emit_mov_reg_reg(&mut asm, 1, RDI, RAX);
    emit_add_reg_reg(&mut asm, 1, RSI, RAX);
    emit_retq(&mut asm);

    let mem: Memory = get_executable_memory(&asm);

    let ptr = mem.ptr();

    let mut cs = Capstone::new().x86()
                                .mode(arch::x86::ArchMode::Mode64)
                                .syntax(arch::x86::ArchSyntax::Att)
                                .detail(true)
                                .build()
                                .unwrap();

    let buf = unsafe { ::std::slice::from_raw_parts(ptr, mem.size()) };

    let i = cs.disasm_all(buf, 0x0).unwrap();
    for i in i.iter() {
        println!("{}", i);
    }

    let f: fn(i32, i32) -> i32 = unsafe { ::std::mem::transmute(ptr) };
    print!("{}\n", f(2, 2));
}

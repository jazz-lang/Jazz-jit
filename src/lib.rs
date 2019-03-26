#![feature(repr_simd)]
#![allow(non_camel_case_types)]
#![allow(non_upper_case_globals)]

pub mod assembler;
pub mod assembler_x64;
pub mod avx;
pub mod constants_x64;
pub mod dseg;

pub mod utils;
pub use self::utils::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(C)]
pub enum MachineMode {
    Int8,
    Int32,
    Int64,
    Float32,
    Float64,
    Ptr,
}

impl MachineMode {
    pub extern "C" fn size(self) -> usize {
        match self {
            MachineMode::Int8 => 1,
            MachineMode::Int32 => 4,
            MachineMode::Int64 => 8,
            MachineMode::Ptr => 8,
            MachineMode::Float32 => 4,
            MachineMode::Float64 => 8,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
#[repr(C)]
pub enum CondCode {
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
}

const PAGE_SIZE: usize = 4096;

use core::mem;

#[cfg(target_family = "unix")]
fn setup(size: usize) -> *mut u8 {
    unsafe {
        let size = size * PAGE_SIZE;
        let mut content: *mut libc::c_void = mem::uninitialized();
        libc::posix_memalign(&mut content, 4096, size);
        let result = libc::mmap(content,
                                size,
                                libc::PROT_EXEC | libc::PROT_READ | libc::PROT_WRITE,
                                libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
                                -1,
                                0);
        mem::transmute(result)
    }
}

#[cfg(target_family = "windows")]
fn setup(size: usize) -> *mut u8 {
    unsafe {
        let _size = size * PAGE_SIZE;

        let mem: *mut u8 = mem::transmute(winapi::um::memoryapi::VirtualAlloc(
            ::std::ptr::null_mut(),
            _size,
            winapi::um::winnt::MEM_COMMIT,
            winapi::um::winnt::PAGE_EXECUTE_READWRITE,
        ));
        mem
    }
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct Memory {
    start: *const u8,
    end: *const u8,

    pointer: *const u8,
    size: usize,
}

impl Memory {
    pub extern "C" fn new(ptr: *const u8) -> Memory {
        Memory { start: unsafe { ptr.offset(0) },
                 end: ptr,
                 pointer: ptr,
                 size: 0xdead }
    }
    pub extern "C" fn start(&self) -> *const u8 { self.start }

    pub extern "C" fn end(&self) -> *const u8 { self.end }

    pub extern "C" fn ptr(&self) -> *const u8 { self.pointer }

    pub extern "C" fn size(&self) -> usize { self.size }
}

use self::assembler::Assembler;

#[no_mangle]
pub extern "C" fn get_executable_memory(buf: &Assembler) -> Memory {
    let data = copy_vec(buf.data());
    let dseg = &buf.dseg;
    let total_size = data.len() + dseg.size() as usize;
    let ptr = setup(total_size);

    dseg.finish(ptr);

    let start;
    unsafe {
        start = ptr.offset(dseg.size() as isize);
        ::core::ptr::copy_nonoverlapping(data.as_ptr(), start as *mut u8, data.len());
    };

    let memory = Memory { start,
                          end: unsafe { ptr.offset(total_size as isize) },
                          pointer: ptr,
                          size: total_size };

    memory
}


pub mod c_api {
    use crate::constants_x64::*;
    use crate::assembler::Mem;
    #[no_mangle]
    
    pub extern "C" fn mem_base(reg: Register,off: i32) -> Mem {
        return Mem::Base(reg,off);
    }
    #[no_mangle]
    pub extern "C" fn mem_local(off: i32) -> Mem {
        return Mem::Local(off);
    } 
    #[no_mangle]
    pub extern "C" fn mem_offset(reg: Register,v1: i32,v2: i32) -> Mem {
        Mem::Offset(reg,v1,v2)
    }
    #[no_mangle]
    pub extern "C" fn mem_index(reg: Register,reg2: Register,v1: i32,v2: i32) -> Mem {
        Mem::Index(reg,reg2,v1,v2)
    }
}
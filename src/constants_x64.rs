#[derive(Clone, Debug, PartialEq, Eq, Copy, PartialOrd, Ord,Hash)]
#[repr(i32)]
pub enum Register {
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
}

impl Register {
    pub fn is_basic_reg(self) -> bool {
        self == RAX || self == RBX || self == RCX || self == RDX
    }
    pub fn msb(self) -> u8 {
        assert!(self != RIP);

        (self as u8 >> 3) & 0x01
    }

    pub fn and7(self) -> u8 {
        assert!(self != RIP);

        self as u8 & 0x07
    }
}



pub use self::Register::*;

#[derive(Clone, Debug, PartialEq, Eq, Copy, PartialOrd, Ord,Hash)]
#[repr(i32)]
pub enum ByteRegister {
    AL = 0,
    CL = 1,
    DL = 2,
    BL = 3,
    AH = 4,
    CH = 5,
    DH = 6,
    BH = 7,
    SPL = 4 | 0x10,
    BPL = 5 | 0x10,
    SIL = 6 | 0x10,
    DIL = 7 | 0x10,
    R8B = 8,
    R9B = 9,
    R10B = 10,
    R11B = 11,
    R12B = 12,
    R13B = 13,
    R14B = 14,
    R15B = 15,
    /// Signals an illegal register.
    kNoByteRegister = -1,
}

pub use self::ByteRegister::*;

#[inline]
pub fn byte_register_of(reg: Register) -> ByteRegister {
    if RSP <= reg && reg <= RDI {
        return unsafe { ::core::mem::transmute(reg as i32 | 0x10) };
    } else {
        return unsafe { ::core::mem::transmute(reg) };
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Copy, PartialOrd, Ord,Hash)]
#[repr(i32)]
pub enum XMMRegister {
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
    kNoXmmRegister = -1, // Signals an illegal register.
}

impl XMMRegister {
    pub fn msb(self) -> u8 {
        //assert!(self != RIP);

        (self as u8 >> 3) & 0x01
    }

    pub fn and7(self) -> u8 {
        //assert!(self != RIP);

        self as u8 & 0x07
    }
}

pub use self::XMMRegister::*;

pub type FpuRegister = XMMRegister;
pub const FpuTMP: FpuRegister = XMM0;

#[derive(Clone, Debug, PartialEq, Eq, Copy, PartialOrd, Ord)]
#[repr(i32)]
pub enum RexBits {
    REX_NONE = 0,
    REX_B = 1 << 0,
    REX_X = 1 << 1,
    REX_R = 1 << 2,
    REX_W = 1 << 3,
    REX_PREFIX = 1 << 6,
}

pub use self::RexBits::*;

pub const TMP: Register = R11;
pub const TMP2: Register = R10;
pub const PP: Register = R15;
/// Stack pointer register
pub const SPREG: Register = RSP;
/// Frame pointer register
pub const FPREG: Register = RBP;


pub const kExceptionObjectReg: Register = RAX;
pub const kStackTraceObjectReg: Register = RDX;



#[derive(Clone, Debug, PartialEq, Eq, Copy, PartialOrd, Ord,Hash)]
pub enum Reg {
    Gpr(Register),
    Byte(ByteRegister),
    Float(XMMRegister),
}

impl Reg {
    pub fn reg(&self) -> Register {
        match self {
            Reg::Gpr(reg) => *reg,
            _ => panic!("")
        }
    }

    pub fn breg(&self) -> ByteRegister {
        match self{
            Reg::Byte(breg) => *breg,
            _ => panic!("")
        }
    }

    pub fn freg(&self) -> XMMRegister {
        match self {
            Reg::Float(float) => *float,
            _ => panic!("")
        }
    }
}

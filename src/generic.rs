/*pub trait GenericEmiter2Arg<X,Y> {
    fn mov(&mut self,x64: bool,_: X,_: Y);
    fn add(&mut self,x64: bool,_: X,_: Y);
    fn sub(&mut self,x64: bool,_: X,_: Y);
    
}
*/


use crate::assembler::*;
use crate::assembler_x64::*;
use crate::*;
use crate::avx::*;
use crate::constants_x64::*;


macro_rules! generic_gen {
    (
        trait $trait_name : ident < $($x: ident),* > {
            $(
            fn $fun_name: ident ( &mut self,$($a: tt),*);
            )*
        }

        impl {
            $(($($tname: tt),*) => $(fn $fname: ident (&mut self,$($arg: ident : $typ: tt),*) {$call: ident ($($argc: expr),*) } ),*)*
        }

    ) => {
        pub trait $trait_name < $($x),*> {
            $(
                fn $fun_name (&mut self,$(_: $a),*) {unimplemented!()}
            )*
        }

        $(
        impl $trait_name< $($tname),*> for Assembler {
            $(
                fn $fname (&mut self, $($arg: $typ),*) {
                    $call(self,$($argc),*);
                }
            )*
        }

        )*
    };

    (expand ( $($tname: tt),*) =>  $($fnname : ident  ($($arg: ident ),*) => $call: ident ($($argc: expr),*)  )*) => {
        $(
            fn $fnname (&mut self,$($arg: $tname),*) {
                $call (self,$($argc),*)
            }
        )*
    }
}
generic_gen! (
    trait PushPop<X> {
        fn push(&mut self,X);
        fn pop(&mut self,X);
    }
    impl {
        (Register) => 
            fn push(&mut self,r: Register) {emit_pushq_reg(r) },
            fn pop(&mut self,r: Register) {emit_popq_reg(r)}   
    }
);


generic_gen!(
    trait Mov<X,Y> {
        fn mov(&mut self,bool,X,Y);
    }

    impl {
        (Register,Register) => 
            fn mov(&mut self,x64: bool,r1: Register,r2: Register) {
                emit_mov_reg_reg(x64 as u8, r1, r2)
            }
        (i32,Register) => 
            fn mov(&mut self,x64: bool,r1: i32,r2: Register) {
                emit_movl_imm_reg(r1 as i32, r2)
            }
        (i64,Register) => 
            fn mov(&mut self,x64: bool,r1: i64,r2: Register) {
                emit_movq_imm64_reg(r1, r2)
            }

        (XMMRegister,XMMRegister) => 
            fn mov(&mut self,x64: bool,r1: XMMRegister,r2: XMMRegister) {
                movsd(r1,r2)
            }
        (XMMRegister,Register) => 
            fn mov(&mut self,x64: bool,r1: XMMRegister,r2: Register) {
                movq_freg_reg(r1, r2)
            }
        
    }
);

generic_gen!(
    trait Add<X,Y> {
        fn add(&mut self,bool,X,Y);
    }

    impl {
        (Register,Register) => 
            fn add(&mut self,x64: bool,r1: Register,r2: Register) {
                emit_add_reg_reg(x64 as u8, r1, r2)
            }

        (XMMRegister,XMMRegister) => 
            fn add(&mut self,x64: bool,r1: XMMRegister,r2: XMMRegister) {
                addsd(r1,r2)
            }
        
        (i32,Register) => 
            fn add(&mut self,x64: bool, r1: i32,r2: Register) {
                emit_addq_imm_reg(r1, r2)
            }
    }
);

generic_gen!(
    trait Sub<X,Y> {
        fn sub(&mut self,bool,X,Y);
    }
    impl {
        (Register,Register) => 
            fn sub(&mut self,x64: bool,r1: Register,r2: Register) {
                emit_sub_reg_reg(x64 as u8, r1, r2)
            }
        (XMMRegister,XMMRegister) => 
            fn sub(&mut self,x64: bool,r1: XMMRegister,r2: XMMRegister) {
                subsd(r1,r2)
            }
        (i32,Register) => 
            fn sub(&mut self,x64: bool,r1: i32,r2: Register) {
                emit_subq_imm_reg(r1, r2)
            }
       
    }
);

generic_gen!(
    trait Ret<> {
        fn ret(&mut self,);
    }

    impl {
        () => 
            fn ret(&mut self,) {
                emit_retq()
            }
    }
);

generic_gen!(
    trait Call<X> {
        fn call(&mut self,X);
    }

    impl {
        (Register) => 
            fn call(&mut self,r: Register) {
                emit_callq_reg(r)
            }
        
    }
);
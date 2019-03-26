pub extern "C" fn copy_vec<T: Copy>(v: &Vec<T>) -> Vec<T> {
    let mut new = Vec::new();

    for value in v {
        new.push(*value);
    }
    new
}

pub extern "C" fn align(value: i32, align: i32) -> i32 {
    if align == 0 {
        return value;
    }

    ((value + align - 1) / align) * align
}

#[macro_export]
macro_rules!  fast_const {
    ($base:ident: $($v: vis $name:ident = $e:expr),+) => {
        $(
            $v const $name: $base = $e;
        )*
    };
}

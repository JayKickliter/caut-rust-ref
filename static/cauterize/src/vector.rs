pub trait Vector: Sized {
    type T: Sized;
    const CAPACITY: usize;
    fn new() -> Self;
    fn push(&mut self, elem: Self::T) -> Option<Self::T>;
}

#[macro_export]
macro_rules! impl_vector {
    ($name:ident, $eltype:ident, $capacity:expr) => (
        pub struct $name {
            len: usize,
            elems: [$eltype;$capacity],
        }

        impl Vector for $name {
            type T = $eltype;
            const CAPACITY: usize = $capacity;
            fn new() -> $name {
                use std::mem;
                $name {
                    len: 0,
                    elems: unsafe { mem::uninitialized() },
                }
            }

            fn push(&mut self, elem: $eltype) -> Option<$eltype> {
                if self.len == $name::CAPACITY {
                    return Some(elem);
                }
                self.elems[self.len] = elem;
                self.len += 1;
                None
            }
        }
   )
}

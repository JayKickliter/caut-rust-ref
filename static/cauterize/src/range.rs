use error;

pub trait Range: Sized {
    type P;
    type T;
    const OFFSET: Self::P;
    const LENGTH: Self::P;
    fn new(val: Self::P) -> Result<Self, error::Error>;
    fn set(&mut self, val: Self::P) -> Option<Self::P>;
    fn get(&self) -> Self::P;
}

#[macro_export]
macro_rules! impl_range {
    ($name:ident, $rep_type:ty, $tag_type:ty, $offset:expr, $length:expr) => (
        pub struct $name($rep_type);

        impl Range for $name {
            type P = $rep_type;
            type T = $tag_type;
            const OFFSET: $rep_type = $offset;
            const LENGTH: $rep_type = $length;
            fn new(val: Self::P) -> Result<Self,Error> {
                if (Self::OFFSET <= val) && (val <= Self::OFFSET + Self::LENGTH) {
                    return Ok($name(val));
                }
                Err(Error::OutOfRange)
            }
            fn set(&mut self, val: Self::P) -> Option<Self::P> {
                if (Self::OFFSET <= val) && (val <= Self::OFFSET + Self::LENGTH) {
                    self.0 = val;
                    return None;
                }
                Some(val)
            }
            fn get(&self) -> Self::P {
                self.0
            }
        }
    )
}

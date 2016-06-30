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

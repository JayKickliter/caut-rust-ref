use std::io;

pub enum CautError {
    Error,
}

pub struct Decode<'a> {
    pos: usize,
    buf: &'a [u8],
}

pub struct Encode<'a> {
    pos: usize,
    buf: &'a [u8],
}


pub trait Cauterize: 'static+Sized {
    fn decode(&mut Decode) -> Result<Self, CautError>;
    fn encode(&self, &mut Encode) -> Result<(), CautError>;
}



#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}

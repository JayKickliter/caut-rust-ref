extern crate byteorder;
use std::io::{Write, Read, Cursor};
use self::byteorder::{ReadBytesExt, WriteBytesExt, LittleEndian};

/*************************/
/* Core types and traits */
/*************************/

#[derive(Debug)]
pub enum Error {
    Encode,
    Decode,
    InvalidTag,
    InvalidValue,
    ElementCount,
    OutOfRange
}

type CautEndian = LittleEndian;

pub struct Decoder {
    pub csr: Cursor<Vec<u8>>,
}
impl Decoder {
    pub fn new(buf: Vec<u8>) -> Self {
        Decoder { csr: Cursor::new(buf) }
    }
}

pub struct Encoder {
    pub csr: Cursor<Vec<u8>>,
}
impl Encoder {
    pub fn new(buf: Vec<u8>) -> Self {
        Encoder { csr: Cursor::new(buf) }
    }

    pub fn consume(self) -> Vec<u8> {
        self.csr.into_inner()
    }
}


pub trait Cauterize: 'static + Sized {
    fn decode(&mut Decoder) -> Result<Self, Error>;
    fn encode(&self, &mut Encoder) -> Result<(), Error>;
}


pub trait Range: Sized {
    type P;
    type T;
    const OFFSET: Self::P;
    const LENGTH: Self::P;
    fn new(val: Self::P) -> Result<Self,Error>;
    fn set(&mut self, val: Self::P) -> Option<Self::P>;
    fn get(&self) -> Self::P;
}

/*******************/
/* Primitive impls */
/*******************/

impl Cauterize for u8 {
    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        let buf: &mut [u8] = &mut [0];
        match ctx.csr.read(buf) {
            Result::Ok(_) => Result::Ok(buf[0]),
            Result::Err(_) => Result::Err(Error::Decode),
        }

    }

    fn encode(&self, enc: &mut Encoder) -> Result<(), Error> {
        let buf: &[u8] = &[*self];
        match enc.csr.write(buf) {
            Result::Ok(1) => Result::Ok(()),
            _ => Result::Err(Error::Encode),
        }
    }
}

impl Cauterize for i8 {
    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        let buf: &mut [u8] = &mut [0];
        match ctx.csr.read(buf) {
            Result::Ok(_) => Result::Ok(buf[0] as i8),
            Result::Err(_) => Result::Err(Error::Decode),
        }
    }

    fn encode(&self, enc: &mut Encoder) -> Result<(), Error> {
        let buf: &[u8] = &[*self as u8];
        match enc.csr.write(buf) {
            Result::Ok(1) => Result::Ok(()),
            _ => Result::Err(Error::Encode),
        }
    }
}


impl Cauterize for u16 {
    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        match ctx.csr.read_u16::<CautEndian>() {
            Result::Ok(val) => Result::Ok(val),
            Result::Err(_) => Result::Err(Error::Decode),
        }
    }

    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        match ctx.csr.write_u16::<CautEndian>(*self) {
            Result::Ok(()) => Result::Ok(()),
            _ => Result::Err(Error::Encode),
        }
    }
}

impl Cauterize for i16 {
    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        match ctx.csr.read_i16::<CautEndian>() {
            Result::Ok(val) => Result::Ok(val),
            Result::Err(_) => Result::Err(Error::Decode),
        }
    }

    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        match ctx.csr.write_i16::<CautEndian>(*self) {
            Result::Ok(()) => Result::Ok(()),
            _ => Result::Err(Error::Encode),
        }
    }
}

impl Cauterize for u32 {
    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        match ctx.csr.read_u32::<CautEndian>() {
            Result::Ok(val) => Result::Ok(val),
            Result::Err(_) => Result::Err(Error::Decode),
        }
    }

    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        match ctx.csr.write_u32::<CautEndian>(*self) {
            Result::Ok(()) => Result::Ok(()),
            _ => Result::Err(Error::Encode),
        }
    }
}

impl Cauterize for i32 {
    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        match ctx.csr.read_i32::<CautEndian>() {
            Result::Ok(val) => Result::Ok(val),
            Result::Err(_) => Result::Err(Error::Decode),
        }
    }

    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        match ctx.csr.write_i32::<CautEndian>(*self) {
            Result::Ok(()) => Result::Ok(()),
            _ => Result::Err(Error::Encode),
        }
    }
}

impl Cauterize for u64 {
    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        match ctx.csr.read_u64::<CautEndian>() {
            Result::Ok(val) => Result::Ok(val),
            Result::Err(_) => Result::Err(Error::Decode),
        }
    }

    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        match ctx.csr.write_u64::<CautEndian>(*self) {
            Result::Ok(()) => Result::Ok(()),
            _ => Result::Err(Error::Encode),
        }
    }
}

impl Cauterize for i64 {
    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        match ctx.csr.read_i64::<CautEndian>() {
            Result::Ok(val) => Result::Ok(val),
            Result::Err(_) => Result::Err(Error::Decode),
        }
    }

    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        match ctx.csr.write_i64::<CautEndian>(*self) {
            Result::Ok(()) => Result::Ok(()),
            _ => Result::Err(Error::Encode),
        }
    }
}

impl Cauterize for f32 {
    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        match ctx.csr.read_f32::<CautEndian>() {
            Result::Ok(val) => Result::Ok(val),
            Result::Err(_) => Result::Err(Error::Decode),
        }
    }

    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        match ctx.csr.write_f32::<CautEndian>(*self) {
            Result::Ok(()) => Result::Ok(()),
            _ => Result::Err(Error::Encode),
        }
    }
}

impl Cauterize for f64 {
    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        match ctx.csr.read_f64::<CautEndian>() {
            Result::Ok(val) => Result::Ok(val),
            Result::Err(_) => Result::Err(Error::Decode),
        }
    }

    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        match ctx.csr.write_f64::<CautEndian>(*self) {
            Result::Ok(()) => Result::Ok(()),
            _ => Result::Err(Error::Encode),
        }
    }
}

impl Cauterize for bool {
    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        match u8::decode(ctx) {
            Ok(0) => Ok(false),
            Ok(1) => Ok(true),
            Err(e) => Err(e),
            _ => Err(Error::InvalidValue),
        }
    }

    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        let val = *self as u8;
        val.encode(ctx)
    }
}



#[cfg(test)]
mod tests {
    use super::*;
    extern crate quickcheck;
    use self::quickcheck::*;

    #[derive(Clone,Debug,PartialEq)]
    enum CautPrim {
        U8(u8),
        I8(i8),
        U16(u16),
        I16(i16),
        U32(u32),
        I32(i32),
        U64(u64),
        I64(i64),
        F32(f32),
        F64(f64),
    }

    impl Cauterize for CautPrim {
        fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
            let tag = try!(u8::decode(ctx));
            match tag {
                0 => Ok(CautPrim::U8(try!(u8::decode(ctx)))),
                1 => Ok(CautPrim::I8(try!(i8::decode(ctx)))),
                2 => Ok(CautPrim::U16(try!(u16::decode(ctx)))),
                3 => Ok(CautPrim::I16(try!(i16::decode(ctx)))),
                4 => Ok(CautPrim::U32(try!(u32::decode(ctx)))),
                5 => Ok(CautPrim::I32(try!(i32::decode(ctx)))),
                6 => Ok(CautPrim::U64(try!(u64::decode(ctx)))),
                7 => Ok(CautPrim::I64(try!(i64::decode(ctx)))),
                8 => Ok(CautPrim::F32(try!(f32::decode(ctx)))),
                9 => Ok(CautPrim::F64(try!(f64::decode(ctx)))),
                _ => Err(Error::InvalidTag),
            }
        }

        fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
            match self {
                &CautPrim::U8(ref val) => {
                    let tag: u8 = 0;
                    try!(tag.encode(ctx));
                    try!(val.encode(ctx));
                }
                &CautPrim::I8(ref val) => {
                    let tag: u8 = 1;
                    try!(tag.encode(ctx));
                    try!(val.encode(ctx));
                }
                &CautPrim::U16(ref val) => {
                    let tag: u8 = 2;
                    try!(tag.encode(ctx));
                    try!(val.encode(ctx));
                }
                &CautPrim::I16(ref val) => {
                    let tag: u8 = 3;
                    try!(tag.encode(ctx));
                    try!(val.encode(ctx));
                }
                &CautPrim::U32(ref val) => {
                    let tag: u8 = 4;
                    try!(tag.encode(ctx));
                    try!(val.encode(ctx));
                }
                &CautPrim::I32(ref val) => {
                    let tag: u8 = 5;
                    try!(tag.encode(ctx));
                    try!(val.encode(ctx));
                }
                &CautPrim::U64(ref val) => {
                    let tag: u8 = 6;
                    try!(tag.encode(ctx));
                    try!(val.encode(ctx));
                }
                &CautPrim::I64(ref val) => {
                    let tag: u8 = 7;
                    try!(tag.encode(ctx));
                    try!(val.encode(ctx));
                }
                &CautPrim::F32(ref val) => {
                    let tag: u8 = 8;
                    try!(tag.encode(ctx));
                    try!(val.encode(ctx));
                }
                &CautPrim::F64(ref val) => {
                    let tag: u8 = 9;
                    try!(tag.encode(ctx));
                    try!(val.encode(ctx));
                }
            };
            Ok(())
        }
    }

    impl Arbitrary for CautPrim {
        fn arbitrary<G: Gen>(g: &mut G) -> CautPrim {
            let arm = g.gen::<usize>() % 10;
            match arm {
                0 => CautPrim::U8(g.gen()),
                1 => CautPrim::I8(g.gen()),
                2 => CautPrim::U16(g.gen()),
                3 => CautPrim::I16(g.gen()),
                4 => CautPrim::U32(g.gen()),
                5 => CautPrim::I32(g.gen()),
                6 => CautPrim::U64(g.gen()),
                7 => CautPrim::I64(g.gen()),
                8 => CautPrim::F32(g.gen()),
                _ => CautPrim::F64(g.gen()),
            }
        }
    }

    #[quickcheck]
    fn caut_prim_round_trip(items: Vec<CautPrim>) -> bool {
        let buf: Vec<u8> = Vec::new();
        let mut ctx = Encoder::new(buf);
        for item in &items {
            item.encode(&mut ctx).unwrap();
        }

        let buf = ctx.consume();
        let mut ctx = Decoder::new(buf);
        let mut decoded: Vec<CautPrim> = Vec::new();
        for _ in 0..items.len() {
            &decoded.push(CautPrim::decode(&mut ctx).unwrap());
        }
        decoded == items
    }
}

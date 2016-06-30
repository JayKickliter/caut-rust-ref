extern crate byteorder;
use self::byteorder::{ReadBytesExt, WriteBytesExt, LittleEndian};
use std::io::{Write, Read, Cursor};

use error::Error;

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
    const FINGERPRINT: [u8; 20];
    const SIZE_MIN: usize;
    const SIZE_MAX: usize;
    fn decode(&mut Decoder) -> Result<Self, Error>;
    fn encode(&self, &mut Encoder) -> Result<(), Error>;
}

pub trait Primitive: 'static + Sized {
    fn decode(&mut Decoder) -> Result<Self, Error>;
    fn encode(&self, &mut Encoder) -> Result<(), Error>;
}



// ****************
// Primitive impls
// ****************

impl Primitive for u8 {
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

impl Primitive for i8 {
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

impl Primitive for u16 {
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

impl Primitive for i16 {
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

impl Primitive for u32 {
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

impl Primitive for i32 {
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

impl Primitive for u64 {
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

impl Primitive for i64 {
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

impl Primitive for f32 {
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

impl Primitive for f64 {
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

impl Primitive for bool {
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

extern crate byteorder;
use self::byteorder::{ReadBytesExt, WriteBytesExt, LittleEndian};
use std::io::{Write, Read};

use error::Error;

pub type Encoder = Write;
pub type Decoder = Read;

type CautEndian = LittleEndian;

pub trait Cauterize: 'static + Sized {
    const FINGERPRINT: [u8; 20];
    const SIZE_MIN: usize;
    const SIZE_MAX: usize;
    fn encode(&self, &mut Encoder) -> Result<(), Error>;
    fn decode(&mut Decoder) -> Result<Self, Error>;
}

pub trait Primitive: 'static + Sized {
    fn encode(&self, &mut Encoder) -> Result<(), Error>;
    fn decode(&mut Decoder) -> Result<Self, Error>;
}


// ****************
// Primitive impls
// ****************

impl Primitive for u8 {
    fn encode(&self, enc: &mut Encoder) -> Result<(), Error> {
        match enc.write_u8(*self) {
            Result::Ok(()) => Result::Ok(()),
            _ => Result::Err(Error::Encode),
        }
    }
    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        match ctx.read_u8() {
            Result::Ok(val) => Result::Ok(val),
            Result::Err(_) => Result::Err(Error::Decode),
        }

    }
}

impl Primitive for i8 {
    fn encode(&self, enc: &mut Encoder) -> Result<(), Error> {
        match enc.write_i8(*self) {
            Result::Ok(()) => Result::Ok(()),
            _ => Result::Err(Error::Encode),
        }
    }
    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        match ctx.read_i8() {
            Result::Ok(val) => Result::Ok(val),
            Result::Err(_) => Result::Err(Error::Decode),
        }

    }
}

impl Primitive for u16 {
    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        match ctx.write_u16::<CautEndian>(*self) {
            Result::Ok(()) => Result::Ok(()),
            _ => Result::Err(Error::Encode),
        }
    }

    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        match ctx.read_u16::<CautEndian>() {
            Result::Ok(val) => Result::Ok(val),
            Result::Err(_) => Result::Err(Error::Decode),
        }
    }
}

impl Primitive for i16 {
    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        match ctx.write_i16::<CautEndian>(*self) {
            Result::Ok(()) => Result::Ok(()),
            _ => Result::Err(Error::Encode),
        }
    }

    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        match ctx.read_i16::<CautEndian>() {
            Result::Ok(val) => Result::Ok(val),
            Result::Err(_) => Result::Err(Error::Decode),
        }
    }
}

impl Primitive for u32 {
    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        match ctx.write_u32::<CautEndian>(*self) {
            Result::Ok(()) => Result::Ok(()),
            _ => Result::Err(Error::Encode),
        }
    }

    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        match ctx.read_u32::<CautEndian>() {
            Result::Ok(val) => Result::Ok(val),
            Result::Err(_) => Result::Err(Error::Decode),
        }
    }
}

impl Primitive for i32 {
    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        match ctx.write_i32::<CautEndian>(*self) {
            Result::Ok(()) => Result::Ok(()),
            _ => Result::Err(Error::Encode),
        }
    }

    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        match ctx.read_i32::<CautEndian>() {
            Result::Ok(val) => Result::Ok(val),
            Result::Err(_) => Result::Err(Error::Decode),
        }
    }
}

impl Primitive for u64 {
    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        match ctx.write_u64::<CautEndian>(*self) {
            Result::Ok(()) => Result::Ok(()),
            _ => Result::Err(Error::Encode),
        }
    }

    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        match ctx.read_u64::<CautEndian>() {
            Result::Ok(val) => Result::Ok(val),
            Result::Err(_) => Result::Err(Error::Decode),
        }
    }
}

impl Primitive for i64 {
    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        match ctx.write_i64::<CautEndian>(*self) {
            Result::Ok(()) => Result::Ok(()),
            _ => Result::Err(Error::Encode),
        }
    }

    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        match ctx.read_i64::<CautEndian>() {
            Result::Ok(val) => Result::Ok(val),
            Result::Err(_) => Result::Err(Error::Decode),
        }
    }
}

impl Primitive for f32 {
    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        match ctx.write_f32::<CautEndian>(*self) {
            Result::Ok(()) => Result::Ok(()),
            _ => Result::Err(Error::Encode),
        }
    }

    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        match ctx.read_f32::<CautEndian>() {
            Result::Ok(val) => Result::Ok(val),
            Result::Err(_) => Result::Err(Error::Decode),
        }
    }
}

impl Primitive for f64 {
    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        match ctx.write_f64::<CautEndian>(*self) {
            Result::Ok(()) => Result::Ok(()),
            _ => Result::Err(Error::Encode),
        }
    }

    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        match ctx.read_f64::<CautEndian>() {
            Result::Ok(val) => Result::Ok(val),
            Result::Err(_) => Result::Err(Error::Decode),
        }
    }
}

impl Primitive for bool {
    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        let val = *self as u8;
        val.encode(ctx)
    }

    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        match u8::decode(ctx) {
            Ok(0) => Ok(false),
            Ok(1) => Ok(true),
            Err(e) => Err(e),
            _ => Err(Error::InvalidValue),
        }

    }
}

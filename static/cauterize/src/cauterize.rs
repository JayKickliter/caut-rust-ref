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

macro_rules! impl_primitive {
    ($T:ty, $FR:ident, $FW:ident) => (
        impl Primitive for $T {
            fn encode(&self, enc: &mut Encoder) -> Result<(), Error> {
                match enc.$FW::<CautEndian>(*self) {
                    Result::Ok(()) => Result::Ok(()),
                    _ => Result::Err(Error::Encode),
                }
            }
            fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
                match ctx.$FR::<CautEndian>() {
                    Result::Ok(val) => Result::Ok(val),
                    Result::Err(_) => Result::Err(Error::Decode),
                }
            }
        }
    );
}

impl_primitive!(u16, read_u16, write_u16);
impl_primitive!(i16, read_i16, write_i16);
impl_primitive!(u32, read_u32, write_u32);
impl_primitive!(i32, read_i32, write_i32);
impl_primitive!(u64, read_u64, write_u64);
impl_primitive!(i64, read_i64, write_i64);
impl_primitive!(f32, read_f32, write_f32);
impl_primitive!(f64, read_f64, write_f64);


// We can't use `impl_primitive!` for u8/i8 since it read/write for u8/i8 does not take paramters
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


// We can't use `impl_primitive!` for bool since it
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

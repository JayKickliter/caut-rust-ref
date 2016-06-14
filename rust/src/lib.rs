extern crate byteorder;
use std::io::{Write, Read, Cursor};
use byteorder::{ReadBytesExt, WriteBytesExt, LittleEndian};

#[derive(Debug)]
pub enum Error {
    Encode,
    Decode,
    InvalidTag,
    InvalidValue,
    ElementCount,
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


#[test]
fn test_primitives() {
    // Create encode buffer and context
    let buf: Vec<u8> = Vec::new();
    let mut ectx = Encoder::new(buf);

    // Encode values
    let enc_u8 = 1 as u8;
    let enc_i8 = -1 as i8;
    let enc_u16 = 2 as u16;
    let enc_i16 = -2 as i16;
    let enc_u32 = 3 as u32;
    let enc_i32 = -3 as i32;
    let enc_u64 = 4 as u64;
    let enc_i64 = -4 as i64;
    let enc_f32 = 5.0 as f32;
    let enc_f64 = -5.0 as f64;

    enc_u8.encode(&mut ectx).unwrap();
    enc_i8.encode(&mut ectx).unwrap();
    enc_u16.encode(&mut ectx).unwrap();
    enc_i16.encode(&mut ectx).unwrap();
    enc_u32.encode(&mut ectx).unwrap();
    enc_i32.encode(&mut ectx).unwrap();
    enc_u64.encode(&mut ectx).unwrap();
    enc_i64.encode(&mut ectx).unwrap();
    enc_f32.encode(&mut ectx).unwrap();
    enc_f64.encode(&mut ectx).unwrap();

    // Get the enc buffer back and resuse it for decoding
    let buf = ectx.consume();
    let mut dctx = Decoder::new(buf);

    let dec_u8 = u8::decode(&mut dctx).unwrap();
    let dec_i8 = i8::decode(&mut dctx).unwrap();
    let dec_u16 = u16::decode(&mut dctx).unwrap();
    let dec_i16 = i16::decode(&mut dctx).unwrap();
    let dec_u32 = u32::decode(&mut dctx).unwrap();
    let dec_i32 = i32::decode(&mut dctx).unwrap();
    let dec_u64 = u64::decode(&mut dctx).unwrap();
    let dec_i64 = i64::decode(&mut dctx).unwrap();
    let dec_f32 = f32::decode(&mut dctx).unwrap();
    let dec_f64 = f64::decode(&mut dctx).unwrap();

    assert!(dec_u8 == enc_u8);
    assert!(dec_i8 == enc_i8);
    assert!(dec_u16 == enc_u16);
    assert!(dec_i16 == enc_i16);
    assert!(dec_u32 == enc_u32);
    assert!(dec_i32 == enc_i32);
    assert!(dec_u64 == enc_u64);
    assert!(dec_i64 == enc_i64);
    assert!(dec_f32 == enc_f32);
    assert!(dec_f64 == enc_f64);
}

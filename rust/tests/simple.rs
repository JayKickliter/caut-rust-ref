#![allow(dead_code,unused_variables)]
extern crate cauterize;
use self::cauterize::{Encoder, Decoder, Cauterize};
pub use self::cauterize::Error;
use std::mem;

pub static SPEC_NAME: &'static str = "simple";

#[derive(Debug, Default, PartialEq)]
pub struct Unsigned8(pub u8);

impl Cauterize for Unsigned8 {
    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        let &Unsigned8(ref inner) = self;
        try!(inner.encode(ctx));
        Ok(())
    }

    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        Ok(Unsigned8(try!(u8::decode(ctx))))
    }
}

// Range type not yet implemented.
pub enum SomeRange {
    
}

impl Cauterize for SomeRange {
    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        unimplemented!();
    }

    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        unimplemented!();
    }
}

#[derive(Debug, PartialEq)]
pub enum PrimitiveUnion {
    U8(u8), // 0
    U16(u16), // 1
    U32(u32), // 2
    U64(u64), // 3
    S8(i8), // 4
    S16(i16), // 5
    S32(i32), // 6
    S64(i64), // 7
    Bool(bool), // 8
    F32(f32), // 9
    F64(f64), // 10
}

impl Cauterize for PrimitiveUnion {
    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        match self {
            &PrimitiveUnion::U8(ref val) => {
                let tag: u8 = 0;
                try!(tag.encode(ctx));
                try!(val.encode(ctx));
            }
            &PrimitiveUnion::U16(ref val) => {
                let tag: u8 = 1;
                try!(tag.encode(ctx));
                try!(val.encode(ctx));
            }
            &PrimitiveUnion::U32(ref val) => {
                let tag: u8 = 2;
                try!(tag.encode(ctx));
                try!(val.encode(ctx));
            }
            &PrimitiveUnion::U64(ref val) => {
                let tag: u8 = 3;
                try!(tag.encode(ctx));
                try!(val.encode(ctx));
            }
            &PrimitiveUnion::S8(ref val) => {
                let tag: u8 = 4;
                try!(tag.encode(ctx));
                try!(val.encode(ctx));
            }
            &PrimitiveUnion::S16(ref val) => {
                let tag: u8 = 5;
                try!(tag.encode(ctx));
                try!(val.encode(ctx));
            }
            &PrimitiveUnion::S32(ref val) => {
                let tag: u8 = 6;
                try!(tag.encode(ctx));
                try!(val.encode(ctx));
            }
            &PrimitiveUnion::S64(ref val) => {
                let tag: u8 = 7;
                try!(tag.encode(ctx));
                try!(val.encode(ctx));
            }
            &PrimitiveUnion::Bool(ref val) => {
                let tag: u8 = 8;
                try!(tag.encode(ctx));
                try!(val.encode(ctx));
            }
            &PrimitiveUnion::F32(ref val) => {
                let tag: u8 = 9;
                try!(tag.encode(ctx));
                try!(val.encode(ctx));
            }
            &PrimitiveUnion::F64(ref val) => {
                let tag: u8 = 10;
                try!(tag.encode(ctx));
                try!(val.encode(ctx));
            }
        };
        Ok(())
    }

    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        let tag = try!(u8::decode(ctx));
        match tag {
            0 => Ok(PrimitiveUnion::U8(try!(u8::decode(ctx)))),
            1 => Ok(PrimitiveUnion::U16(try!(u16::decode(ctx)))),
            2 => Ok(PrimitiveUnion::U32(try!(u32::decode(ctx)))),
            3 => Ok(PrimitiveUnion::U64(try!(u64::decode(ctx)))),
            4 => Ok(PrimitiveUnion::S8(try!(i8::decode(ctx)))),
            5 => Ok(PrimitiveUnion::S16(try!(i16::decode(ctx)))),
            6 => Ok(PrimitiveUnion::S32(try!(i32::decode(ctx)))),
            7 => Ok(PrimitiveUnion::S64(try!(i64::decode(ctx)))),
            8 => Ok(PrimitiveUnion::Bool(try!(bool::decode(ctx)))),
            9 => Ok(PrimitiveUnion::F32(try!(f32::decode(ctx)))),
            10 => Ok(PrimitiveUnion::F64(try!(f64::decode(ctx)))),
            _ => Err(Error::InvalidTag),
        }
    }
}

#[derive(Debug, Default, PartialEq)]
pub struct Number64(pub i64);

impl Cauterize for Number64 {
    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        let &Number64(ref inner) = self;
        try!(inner.encode(ctx));
        Ok(())
    }

    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        Ok(Number64(try!(i64::decode(ctx))))
    }
}

#[derive(Debug, Default, PartialEq)]
pub struct SomeArray(pub [Number64; 8]);

impl Cauterize for SomeArray {
    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        let ref elems = self.0;
        for elem in elems.iter() {
            try!(elem.encode(ctx));
        }
        Ok(())
    }

    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        let mut arr: [Number64; 8] = Default::default();
        for i in 0..8 {
            arr[i] = try!(Number64::decode(ctx));
        }
        Ok(SomeArray(arr))
    }
}

#[derive(Debug, Default, PartialEq)]
pub struct SomeVector(pub Vec<Number64>);

impl Cauterize for SomeVector {
    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        unimplemented!();
    }

    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        unimplemented!();
    }
}

#[derive(Debug, PartialEq)]
#[repr(u8)]
pub enum FieldEnum {
    Somearray, // 0
    Somevector, // 1
    Arecord, // 2
}

impl Cauterize for FieldEnum {
    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        let tag: &u8 = unsafe { mem::transmute(self) };
        try!(tag.encode(ctx));
        Ok(())
    }

    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        let tag = try!(u8::decode(ctx));
        if tag > 2 {
            return Err(Error::InvalidTag);
        }
        Ok(unsafe { mem::transmute(tag) })
    }
}

#[derive(Debug, Default, PartialEq)]
pub struct Header(pub Vec<FieldEnum>);

impl Cauterize for Header {
    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        unimplemented!();
    }

    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        unimplemented!();
    }
}

#[derive(Debug, PartialEq)]
#[repr(u8)]
pub enum Color {
    Red, // 0
    Green, // 1
    Blue, // 2
}

impl Cauterize for Color {
    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        let tag: &u8 = unsafe { mem::transmute(self) };
        try!(tag.encode(ctx));
        Ok(())
    }

    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        let tag = try!(u8::decode(ctx));
        if tag > 2 {
            return Err(Error::InvalidTag);
        }
        Ok(unsafe { mem::transmute(tag) })
    }
}

#[derive(Debug, Default, PartialEq)]
pub struct CRecord {
    pub a: i8, // 0
    pub b: i8, // 1
}

impl Cauterize for CRecord {
    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        unimplemented!();
    }

    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        unimplemented!();
    }
}

#[derive(Debug, Default, PartialEq)]
pub struct BRecord {
    pub a: i8, // 0
    pub d: CRecord, // 1
}

impl Cauterize for BRecord {
    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        unimplemented!();
    }

    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        unimplemented!();
    }
}

#[derive(Debug, Default, PartialEq)]
pub struct ARecord {
    pub z: SomeVector, // 0
    pub a: i8, // 1
    pub d: BRecord, // 2
}

impl Cauterize for ARecord {
    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        unimplemented!();
    }

    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        unimplemented!();
    }
}

#[derive(Debug, PartialEq)]
pub enum AUnion {
    A(ARecord), // 0
    B(BRecord), // 1
    C(i8), // 2
    D(Number64), // 3
    E, // 4
}

impl Cauterize for AUnion {
    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        match self {
            &AUnion::A(ref val) => {
                let tag: u8 = 0;
                try!(tag.encode(ctx));
                try!(val.encode(ctx));
            }
            &AUnion::B(ref val) => {
                let tag: u8 = 1;
                try!(tag.encode(ctx));
                try!(val.encode(ctx));
            }
            &AUnion::C(ref val) => {
                let tag: u8 = 2;
                try!(tag.encode(ctx));
                try!(val.encode(ctx));
            }
            &AUnion::D(ref val) => {
                let tag: u8 = 3;
                try!(tag.encode(ctx));
                try!(val.encode(ctx));
            }
            &AUnion::E => {
                let tag: u8 = 4;
                try!(tag.encode(ctx));
            }
        };
        Ok(())
    }

    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        let tag = try!(u8::decode(ctx));
        match tag {
            0 => Ok(AUnion::A(try!(ARecord::decode(ctx)))),
            1 => Ok(AUnion::B(try!(BRecord::decode(ctx)))),
            2 => Ok(AUnion::C(try!(i8::decode(ctx)))),
            3 => Ok(AUnion::D(try!(Number64::decode(ctx)))),
            4 => Ok(AUnion::E),
            _ => Err(Error::InvalidTag),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ACombination {
    pub a: Option<Number64>, // 0
    pub b: Option<i8>, // 1
    pub c: Option<AUnion>, // 2
    pub d: Option<()>, // 3
}

impl Cauterize for ACombination {
    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        unimplemented!();
    }

    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        unimplemented!();
    }
}


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
        unimplemented!();
    }

    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        unimplemented!();
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
        let mut arr: [Number64; 8] = unsafe { mem::uninitialized() };
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
pub enum FieldEnum {
    Somearray, // 0
    Somevector, // 1
    Arecord, // 2
}

impl Cauterize for FieldEnum {
    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        unimplemented!();
    }

    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        unimplemented!();
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
pub enum Color {
    Red, // 0
    Green, // 1
    Blue, // 2
}

impl Cauterize for Color {
    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        unimplemented!();
    }

    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        unimplemented!();
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
        unimplemented!();
    }

    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        unimplemented!();
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


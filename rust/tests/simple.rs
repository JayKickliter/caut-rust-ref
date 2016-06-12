#![allow(dead_code)]
extern crate cauterze;
pub use cauterize::Error;

pub static SPEC_NAME:  &'static str =  "simple";

#[derive(Debug,PartialEq)]
pub struct Unsigned8(u8);

impl Cauterize for Unsigned8 {
    fn encode(ctx: &mut Encoder) -> Result<(), cauterize::Error> {
    
        
    
    }
    fn decode(ctx: &mut Decoder) -> Result<Self, cauterize::Error> {
    
        
    
    }
}

// Range type not yet implemented.
// Not declaring range SomeRange

impl Cauterize for SomeRange {
    fn encode(ctx: &mut Encoder) -> Result<(), cauterize::Error> {
    
        
    
    }
    fn decode(ctx: &mut Decoder) -> Result<Self, cauterize::Error> {
    
        
    
    }
}

#[derive(Debug,PartialEq)]
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
    fn encode(ctx: &mut Encoder) -> Result<(), cauterize::Error> {
    
        
    
    }
    fn decode(ctx: &mut Decoder) -> Result<Self, cauterize::Error> {
    
        
    
    }
}

#[derive(Debug,PartialEq)]
pub struct Number64(i64);

impl Cauterize for Number64 {
    fn encode(ctx: &mut Encoder) -> Result<(), cauterize::Error> {
    
        
    
    }
    fn decode(ctx: &mut Decoder) -> Result<Self, cauterize::Error> {
    
        
    
    }
}

#[derive(Debug,PartialEq)]
pub struct SomeArray([Number64; 8]);

impl Cauterize for SomeArray {
    fn encode(ctx: &mut Encoder) -> Result<(), cauterize::Error> {
    
        
    
    }
    fn decode(ctx: &mut Decoder) -> Result<Self, cauterize::Error> {
    
        
    
    }
}

#[derive(Debug,PartialEq)]
pub struct SomeVector(Vec<Number64>);

impl Cauterize for SomeVector {
    fn encode(ctx: &mut Encoder) -> Result<(), cauterize::Error> {
    
        
    
    }
    fn decode(ctx: &mut Decoder) -> Result<Self, cauterize::Error> {
    
        
    
    }
}

#[derive(Debug,PartialEq)]
pub enum FieldEnum {
    Somearray, // 0
    Somevector, // 1
    Arecord, // 2
}

impl Cauterize for FieldEnum {
    fn encode(ctx: &mut Encoder) -> Result<(), cauterize::Error> {
    
        
    
    }
    fn decode(ctx: &mut Decoder) -> Result<Self, cauterize::Error> {
    
        
    
    }
}

#[derive(Debug,PartialEq)]
pub struct Header(Vec<FieldEnum>);

impl Cauterize for Header {
    fn encode(ctx: &mut Encoder) -> Result<(), cauterize::Error> {
    
        
    
    }
    fn decode(ctx: &mut Decoder) -> Result<Self, cauterize::Error> {
    
        
    
    }
}

#[derive(Debug,PartialEq)]
pub enum Color {
    Red, // 0
    Green, // 1
    Blue, // 2
}

impl Cauterize for Color {
    fn encode(ctx: &mut Encoder) -> Result<(), cauterize::Error> {
    
        
    
    }
    fn decode(ctx: &mut Decoder) -> Result<Self, cauterize::Error> {
    
        
    
    }
}

#[derive(Debug,PartialEq)]
pub struct CRecord {
    pub a: i8, // 0
    pub b: i8, // 1
}

impl Cauterize for CRecord {
    fn encode(ctx: &mut Encoder) -> Result<(), cauterize::Error> {
    
        
    
    }
    fn decode(ctx: &mut Decoder) -> Result<Self, cauterize::Error> {
    
        
    
    }
}

#[derive(Debug,PartialEq)]
pub struct BRecord {
    pub a: i8, // 0
    pub d: CRecord, // 1
}

impl Cauterize for BRecord {
    fn encode(ctx: &mut Encoder) -> Result<(), cauterize::Error> {
    
        
    
    }
    fn decode(ctx: &mut Decoder) -> Result<Self, cauterize::Error> {
    
        
    
    }
}

#[derive(Debug,PartialEq)]
pub struct ARecord {
    pub z: SomeVector, // 0
    pub a: i8, // 1
    pub d: BRecord, // 2
}

impl Cauterize for ARecord {
    fn encode(ctx: &mut Encoder) -> Result<(), cauterize::Error> {
    
        
    
    }
    fn decode(ctx: &mut Decoder) -> Result<Self, cauterize::Error> {
    
        
    
    }
}

#[derive(Debug,PartialEq)]
pub enum AUnion {
    A(ARecord), // 0
    B(BRecord), // 1
    C(i8), // 2
    D(Number64), // 3
    E, // 4
}

impl Cauterize for AUnion {
    fn encode(ctx: &mut Encoder) -> Result<(), cauterize::Error> {
    
        
    
    }
    fn decode(ctx: &mut Decoder) -> Result<Self, cauterize::Error> {
    
        
    
    }
}

#[derive(Debug,PartialEq)]
pub struct ACombination {
    pub a: Option<Number64>, // 0
    pub b: Option<i8>, // 1
    pub c: Option<AUnion>, // 2
    pub d: Option<()>, // 3
}

impl Cauterize for ACombination {
    fn encode(ctx: &mut Encoder) -> Result<(), cauterize::Error> {
    
        
    
    }
    fn decode(ctx: &mut Decoder) -> Result<Self, cauterize::Error> {
    
        
    
    }
}


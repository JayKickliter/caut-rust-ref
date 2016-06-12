extern crate cauterize;
use cauterize::{Error, Cauterize, Encoder, Decoder};
mod simple;
use simple::PrimitiveTest;

impl Cauterize for PrimitiveTest {
    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        let tag = try!(u8::decode(ctx));
        match tag {
            0 => Ok(simple::PrimitiveTest::U8(try!(u8::decode(ctx)))),
            1 => Ok(simple::PrimitiveTest::U16(try!(u16::decode(ctx)))),
            2 => Ok(simple::PrimitiveTest::U32(try!(u32::decode(ctx)))),
            3 => Ok(simple::PrimitiveTest::U64(try!(u64::decode(ctx)))),
            4 => Ok(simple::PrimitiveTest::S8(try!(i8::decode(ctx)))),
            5 => Ok(simple::PrimitiveTest::S16(try!(i16::decode(ctx)))),
            6 => Ok(simple::PrimitiveTest::S32(try!(i32::decode(ctx)))),
            7 => Ok(simple::PrimitiveTest::S64(try!(i64::decode(ctx)))),
            8 => Ok(simple::PrimitiveTest::Bool(try!(bool::decode(ctx)))),
            9 => Ok(simple::PrimitiveTest::F32(try!(f32::decode(ctx)))),
            10 => Ok(simple::PrimitiveTest::F64(try!(f64::decode(ctx)))),
            _ => Err(Error::InvalidTag),
        }
    }

    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        let ret = match self {
            &PrimitiveTest::U8(v) => {
                let tag: u8 = 0;
                try!(tag.encode(ctx));
                try!(v.encode(ctx));
            }
            &PrimitiveTest::U16(v) => {
                let tag: u8 = 1;
                try!(tag.encode(ctx));
                try!(v.encode(ctx));
            }
            &PrimitiveTest::U32(v) => {
                let tag: u8 = 2;
                try!(tag.encode(ctx));
                try!(v.encode(ctx));
            }
            &PrimitiveTest::U64(v) => {
                let tag: u8 = 3;
                try!(tag.encode(ctx));
                try!(v.encode(ctx));
            }
            &PrimitiveTest::S8(v) => {
                let tag: u8 = 4;
                try!(tag.encode(ctx));
                try!(v.encode(ctx));
            }
            &PrimitiveTest::S16(v) => {
                let tag: u8 = 5;
                try!(tag.encode(ctx));
                try!(v.encode(ctx));
            }
            &PrimitiveTest::S32(v) => {
                let tag: u8 = 6;
                try!(tag.encode(ctx));
                try!(v.encode(ctx));
            }
            &PrimitiveTest::S64(v) => {
                let tag: u8 = 7;
                try!(tag.encode(ctx));
                try!(v.encode(ctx));
            }
            &PrimitiveTest::Bool(v) => {
                let tag: u8 = 8;
                try!(tag.encode(ctx));
                try!(v.encode(ctx));
            }
            &PrimitiveTest::F32(v) => {
                let tag: u8 = 9;
                try!(tag.encode(ctx));
                try!(v.encode(ctx));
            }
            &PrimitiveTest::F64(v) => {
                let tag: u8 = 10;
                try!(tag.encode(ctx));
                try!(v.encode(ctx));
            }
        };
        Ok(ret)
    }
}

#[test]
fn main_primitive_test() {
    // Create encode buffer and context
    let buf: Vec<u8> = Vec::new();
    let mut ctx = Encoder::new(buf);

    // Create enum variants
    let enc_pt_u8 = PrimitiveTest::U8(1);
    let enc_pt_u16 = PrimitiveTest::U16(2);
    let enc_pt_u32 = PrimitiveTest::U32(3);
    let enc_pt_u64 = PrimitiveTest::U64(4);
    let enc_pt_i8 = PrimitiveTest::S8(-5);
    let enc_pt_i16 = PrimitiveTest::S16(-6);
    let enc_pt_i32 = PrimitiveTest::S32(-7);
    let enc_pt_i64 = PrimitiveTest::S64(-8);
    let enc_pt_bool = PrimitiveTest::Bool(true);
    let enc_pt_f32 = PrimitiveTest::F32(std::f32::consts::E);
    let enc_pt_f64 = PrimitiveTest::F64(std::f64::consts::PI);

    // Encode enum variants
    enc_pt_u8.encode(&mut ctx).unwrap();
    enc_pt_u16.encode(&mut ctx).unwrap();
    enc_pt_u32.encode(&mut ctx).unwrap();
    enc_pt_u64.encode(&mut ctx).unwrap();
    enc_pt_i8.encode(&mut ctx).unwrap();
    enc_pt_i16.encode(&mut ctx).unwrap();
    enc_pt_i32.encode(&mut ctx).unwrap();
    enc_pt_i64.encode(&mut ctx).unwrap();
    enc_pt_bool.encode(&mut ctx).unwrap();
    enc_pt_f32.encode(&mut ctx).unwrap();
    enc_pt_f64.encode(&mut ctx).unwrap();

    // Decode enum variants
    let buf = ctx.consume();
    let mut ctx = Decoder::new(buf);
    let dec_pt_u8 = PrimitiveTest::decode(&mut ctx).unwrap();
    let dec_pt_u16 = PrimitiveTest::decode(&mut ctx).unwrap();
    let dec_pt_u32 = PrimitiveTest::decode(&mut ctx).unwrap();
    let dec_pt_u64 = PrimitiveTest::decode(&mut ctx).unwrap();
    let dec_pt_i8 = PrimitiveTest::decode(&mut ctx).unwrap();
    let dec_pt_i16 = PrimitiveTest::decode(&mut ctx).unwrap();
    let dec_pt_i32 = PrimitiveTest::decode(&mut ctx).unwrap();
    let dec_pt_i64 = PrimitiveTest::decode(&mut ctx).unwrap();
    let dec_pt_bool = PrimitiveTest::decode(&mut ctx).unwrap();
    let dec_pt_f32 = PrimitiveTest::decode(&mut ctx).unwrap();
    let dec_pt_f64 = PrimitiveTest::decode(&mut ctx).unwrap();

    assert_eq!(enc_pt_u8, dec_pt_u8);
    assert_eq!(enc_pt_u16, dec_pt_u16);
    assert_eq!(enc_pt_u32, dec_pt_u32);
    assert_eq!(enc_pt_u64, dec_pt_u64);
    assert_eq!(enc_pt_i8, dec_pt_i8);
    assert_eq!(enc_pt_i16, dec_pt_i16);
    assert_eq!(enc_pt_i32, dec_pt_i32);
    assert_eq!(enc_pt_i64, dec_pt_i64);
    assert_eq!(enc_pt_bool, dec_pt_bool);
    assert_eq!(enc_pt_f32, dec_pt_f32);
    assert_eq!(enc_pt_f64, dec_pt_f64);
}

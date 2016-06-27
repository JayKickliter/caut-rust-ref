#![cfg_attr(test, feature(plugin))]
#![cfg_attr(test, plugin(quickcheck_macros))]
#![feature(associated_consts)]
#[macro_use]
extern crate quickcheck;
use self::quickcheck::*;
extern crate cauterize;
use cauterize::*;

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
    const FINGERPRINT: [u8; 20] = [0u8; 20];
    const SIZE_MIN: usize = 0;
    const SIZE_MAX: usize = 0;

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

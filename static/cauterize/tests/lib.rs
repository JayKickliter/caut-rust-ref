#[macro_use]
extern crate quickcheck;
extern crate cauterize;
use cauterize::*;
use std::io::Cursor;

#[derive(Clone, Debug, PartialEq)]
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
        let tag = u8::decode(ctx)?;
        match tag {
            0 => Ok(CautPrim::U8(u8::decode(ctx)))?,
            1 => Ok(CautPrim::I8(i8::decode(ctx)))?,
            2 => Ok(CautPrim::U16(u16::decode(ctx)))?,
            3 => Ok(CautPrim::I16(i16::decode(ctx)))?,
            4 => Ok(CautPrim::U32(u32::decode(ctx)))?,
            5 => Ok(CautPrim::I32(i32::decode(ctx)))?,
            6 => Ok(CautPrim::U64(u64::decode(ctx)))?,
            7 => Ok(CautPrim::I64(i64::decode(ctx)))?,
            8 => Ok(CautPrim::F32(f32::decode(ctx)))?,
            9 => Ok(CautPrim::F64(f64::decode(ctx)))?,
            _ => Err(Error::InvalidTag),
        }
    }

    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        match self {
            &CautPrim::U8(ref val) => {
                let tag: u8 = 0;
                tag.encode(ctx)?;
                val.encode(ctx)?;
            }
            &CautPrim::I8(ref val) => {
                let tag: u8 = 1;
                tag.encode(ctx)?;
                val.encode(ctx)?;
            }
            &CautPrim::U16(ref val) => {
                let tag: u8 = 2;
                tag.encode(ctx)?;
                val.encode(ctx)?;
            }
            &CautPrim::I16(ref val) => {
                let tag: u8 = 3;
                tag.encode(ctx)?;
                val.encode(ctx)?;
            }
            &CautPrim::U32(ref val) => {
                let tag: u8 = 4;
                tag.encode(ctx)?;
                val.encode(ctx)?;
            }
            &CautPrim::I32(ref val) => {
                let tag: u8 = 5;
                tag.encode(ctx)?;
                val.encode(ctx)?;
            }
            &CautPrim::U64(ref val) => {
                let tag: u8 = 6;
                tag.encode(ctx)?;
                val.encode(ctx)?;
            }
            &CautPrim::I64(ref val) => {
                let tag: u8 = 7;
                tag.encode(ctx)?;
                val.encode(ctx)?;
            }
            &CautPrim::F32(ref val) => {
                let tag: u8 = 8;
                tag.encode(ctx)?;
                val.encode(ctx)?;
            }
            &CautPrim::F64(ref val) => {
                let tag: u8 = 9;
                tag.encode(ctx)?;
                val.encode(ctx)?;
            }
        };
        Ok(())
    }
}

impl quickcheck::Arbitrary for CautPrim {
    fn arbitrary<G: quickcheck::Gen>(g: &mut G) -> CautPrim {
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

quickcheck! {
    fn caut_prim_round_trip(items: Vec<CautPrim>) -> bool {
        let buf: Vec<u8> = Vec::new();
        let mut ctx = Cursor::new(buf);
        for item in &items {
            item.encode(&mut ctx).unwrap();
        }

        let buf = ctx.into_inner();
        let mut ctx = Cursor::new(buf);
        let mut decoded: Vec<CautPrim> = Vec::new();
        for _ in 0..items.len() {
            &decoded.push(CautPrim::decode(&mut ctx).unwrap());
        }
        decoded == items
    }
}

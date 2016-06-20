#![cfg_attr(test, feature(plugin))]
#![cfg_attr(test, plugin(quickcheck_macros))]
#![allow(dead_code,unused_variables,unused_imports)]
#![feature(associated_consts)]

#[cfg(test)]
#[macro_use]
extern crate quickcheck;
mod cauterize;
pub use self::cauterize::{Primitive, Error, Encoder, Decoder, Cauterize, Range};
use std::mem;

pub static SPEC_NAME: &'static str = "aey";
pub const SPEC_FINGERPRINT: [u8;20] = [0x9a,0xbe,0x04,0xd5,0xee,0x15,0x9d,0xad,0xf0,0x9f,0x6c,0x55,0x97,0x49,0xa0,0x6a,0xea,0x94,0x28,0x59];
pub const SPEC_MIN_SIZE: usize = 1;
pub const SPEC_MAX_SIZE: usize = 13;

pub struct A {
    pub a: Option<()>, // 0
    pub e: Option<()>, // 1
    pub i: Option<()>, // 2
    pub o: Option<()>, // 3
    pub u: Option<()>, // 4
    pub y: Option<i16>, // 5
    pub aa: Option<i32>, // 6
    pub ae: Option<()>, // 7
    pub ai: Option<i32>, // 8
    pub ao: Option<u8>, // 9
}

impl Cauterize for A {
    const FINGERPRINT: [u8;20] = [0xd2,0x50,0x69,0x9a,0x48,0xb0,0x34,0xc0,0x73,0x2f,0xf7,0x1c,0x0e,0x33,0x0a,0x36,0xec,0x58,0xa9,0x39];
    const SIZE_MIN: usize = 2;
    const SIZE_MAX: usize = 13;

    fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
        let mut tag: u16 = 0;
        tag |= (self.a.is_some() as u16) << 0;
        tag |= (self.e.is_some() as u16) << 1;
        tag |= (self.i.is_some() as u16) << 2;
        tag |= (self.o.is_some() as u16) << 3;
        tag |= (self.u.is_some() as u16) << 4;
        tag |= (self.y.is_some() as u16) << 5;
        tag |= (self.aa.is_some() as u16) << 6;
        tag |= (self.ae.is_some() as u16) << 7;
        tag |= (self.ai.is_some() as u16) << 8;
        tag |= (self.ao.is_some() as u16) << 9;
        try!(tag.encode(ctx));
        // No data for field a
        // No data for field e
        // No data for field i
        // No data for field o
        // No data for field u
        match self.y {
            Some(ref a) => try!(a.encode(ctx)),
            None => (),
        }
        match self.aa {
            Some(ref a) => try!(a.encode(ctx)),
            None => (),
        }
        // No data for field ae
        match self.ai {
            Some(ref a) => try!(a.encode(ctx)),
            None => (),
        }
        match self.ao {
            Some(ref a) => try!(a.encode(ctx)),
            None => (),
        }
        Ok(())
    }

    fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
        let tag = try!(u16::decode(ctx));
        let combo = A {
            a: match tag & (1 << 0) == 0 {
                true => None,
                false => Some(()),
            },
            e: match tag & (1 << 1) == 0 {
                true => None,
                false => Some(()),
            },
            i: match tag & (1 << 2) == 0 {
                true => None,
                false => Some(()),
            },
            o: match tag & (1 << 3) == 0 {
                true => None,
                false => Some(()),
            },
            u: match tag & (1 << 4) == 0 {
                true => None,
                false => Some(()),
            },
            y: match tag & (1 << 5) == 0 {
                true => None,
                false => Some(try!(i16::decode(ctx))),
            },
            aa: match tag & (1 << 6) == 0 {
                true => None,
                false => Some(try!(i32::decode(ctx))),
            },
            ae: match tag & (1 << 7) == 0 {
                true => None,
                false => Some(()),
            },
            ai: match tag & (1 << 8) == 0 {
                true => None,
                false => Some(try!(i32::decode(ctx))),
            },
            ao: match tag & (1 << 9) == 0 {
                true => None,
                false => Some(try!(u8::decode(ctx))),
            },
        };
        Ok(combo)
    }
}


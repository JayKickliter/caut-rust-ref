// use cauterize::{Error, Cauterize, Encoder, Decoder};
mod simple;
// use simple::PrimitiveUnion;

// impl Cauterize for PrimitiveUnion {
//     fn decode(ctx: &mut Decoder) -> Result<Self, Error> {
//         let tag = try!(u8::decode(ctx));
//         match tag {
//             0 => Ok(simple::PrimitiveUnion::U8(try!(u8::decode(ctx)))),
//             1 => Ok(simple::PrimitiveUnion::U16(try!(u16::decode(ctx)))),
//             2 => Ok(simple::PrimitiveUnion::U32(try!(u32::decode(ctx)))),
//             3 => Ok(simple::PrimitiveUnion::U64(try!(u64::decode(ctx)))),
//             4 => Ok(simple::PrimitiveUnion::S8(try!(i8::decode(ctx)))),
//             5 => Ok(simple::PrimitiveUnion::S16(try!(i16::decode(ctx)))),
//             6 => Ok(simple::PrimitiveUnion::S32(try!(i32::decode(ctx)))),
//             7 => Ok(simple::PrimitiveUnion::S64(try!(i64::decode(ctx)))),
//             8 => Ok(simple::PrimitiveUnion::Bool(try!(bool::decode(ctx)))),
//             9 => Ok(simple::PrimitiveUnion::F32(try!(f32::decode(ctx)))),
//             10 => Ok(simple::PrimitiveUnion::F64(try!(f64::decode(ctx)))),
//             _ => Err(Error::InvalidTag),
//         }
//     }

//     fn encode(&self, ctx: &mut Encoder) -> Result<(), Error> {
//         let ret = match self {
//             &PrimitiveUnion::U8(v) => {
//                 let tag: u8 = 0;
//                 try!(tag.encode(ctx));
//                 try!(v.encode(ctx));
//             }
//             &PrimitiveUnion::U16(v) => {
//                 let tag: u8 = 1;
//                 try!(tag.encode(ctx));
//                 try!(v.encode(ctx));
//             }
//             &PrimitiveUnion::U32(v) => {
//                 let tag: u8 = 2;
//                 try!(tag.encode(ctx));
//                 try!(v.encode(ctx));
//             }
//             &PrimitiveUnion::U64(v) => {
//                 let tag: u8 = 3;
//                 try!(tag.encode(ctx));
//                 try!(v.encode(ctx));
//             }
//             &PrimitiveUnion::S8(v) => {
//                 let tag: u8 = 4;
//                 try!(tag.encode(ctx));
//                 try!(v.encode(ctx));
//             }
//             &PrimitiveUnion::S16(v) => {
//                 let tag: u8 = 5;
//                 try!(tag.encode(ctx));
//                 try!(v.encode(ctx));
//             }
//             &PrimitiveUnion::S32(v) => {
//                 let tag: u8 = 6;
//                 try!(tag.encode(ctx));
//                 try!(v.encode(ctx));
//             }
//             &PrimitiveUnion::S64(v) => {
//                 let tag: u8 = 7;
//                 try!(tag.encode(ctx));
//                 try!(v.encode(ctx));
//             }
//             &PrimitiveUnion::Bool(v) => {
//                 let tag: u8 = 8;
//                 try!(tag.encode(ctx));
//                 try!(v.encode(ctx));
//             }
//             &PrimitiveUnion::F32(v) => {
//                 let tag: u8 = 9;
//                 try!(tag.encode(ctx));
//                 try!(v.encode(ctx));
//             }
//             &PrimitiveUnion::F64(v) => {
//                 let tag: u8 = 10;
//                 try!(tag.encode(ctx));
//                 try!(v.encode(ctx));
//             }
//         };
//         Ok(ret)
//     }
// }

// #[test]
// fn main_primitive_test() {
//     // Create encode buffer and context
//     let buf: Vec<u8> = Vec::new();
//     let mut ctx = Encoder::new(buf);

//     // Create enum variants
//     let enc_pt_u8 = PrimitiveUnion::U8(1);
//     let enc_pt_u16 = PrimitiveUnion::U16(2);
//     let enc_pt_u32 = PrimitiveUnion::U32(3);
//     let enc_pt_u64 = PrimitiveUnion::U64(4);
//     let enc_pt_i8 = PrimitiveUnion::S8(-5);
//     let enc_pt_i16 = PrimitiveUnion::S16(-6);
//     let enc_pt_i32 = PrimitiveUnion::S32(-7);
//     let enc_pt_i64 = PrimitiveUnion::S64(-8);
//     let enc_pt_bool = PrimitiveUnion::Bool(true);
//     let enc_pt_f32 = PrimitiveUnion::F32(std::f32::consts::E);
//     let enc_pt_f64 = PrimitiveUnion::F64(std::f64::consts::PI);

//     // Encode enum variants
//     enc_pt_u8.encode(&mut ctx).unwrap();
//     enc_pt_u16.encode(&mut ctx).unwrap();
//     enc_pt_u32.encode(&mut ctx).unwrap();
//     enc_pt_u64.encode(&mut ctx).unwrap();
//     enc_pt_i8.encode(&mut ctx).unwrap();
//     enc_pt_i16.encode(&mut ctx).unwrap();
//     enc_pt_i32.encode(&mut ctx).unwrap();
//     enc_pt_i64.encode(&mut ctx).unwrap();
//     enc_pt_bool.encode(&mut ctx).unwrap();
//     enc_pt_f32.encode(&mut ctx).unwrap();
//     enc_pt_f64.encode(&mut ctx).unwrap();

//     // Decode enum variants
//     let buf = ctx.consume();
//     let mut ctx = Decoder::new(buf);
//     let dec_pt_u8 = PrimitiveUnion::decode(&mut ctx).unwrap();
//     let dec_pt_u16 = PrimitiveUnion::decode(&mut ctx).unwrap();
//     let dec_pt_u32 = PrimitiveUnion::decode(&mut ctx).unwrap();
//     let dec_pt_u64 = PrimitiveUnion::decode(&mut ctx).unwrap();
//     let dec_pt_i8 = PrimitiveUnion::decode(&mut ctx).unwrap();
//     let dec_pt_i16 = PrimitiveUnion::decode(&mut ctx).unwrap();
//     let dec_pt_i32 = PrimitiveUnion::decode(&mut ctx).unwrap();
//     let dec_pt_i64 = PrimitiveUnion::decode(&mut ctx).unwrap();
//     let dec_pt_bool = PrimitiveUnion::decode(&mut ctx).unwrap();
//     let dec_pt_f32 = PrimitiveUnion::decode(&mut ctx).unwrap();
//     let dec_pt_f64 = PrimitiveUnion::decode(&mut ctx).unwrap();

//     assert_eq!(enc_pt_u8, dec_pt_u8);
//     assert_eq!(enc_pt_u16, dec_pt_u16);
//     assert_eq!(enc_pt_u32, dec_pt_u32);
//     assert_eq!(enc_pt_u64, dec_pt_u64);
//     assert_eq!(enc_pt_i8, dec_pt_i8);
//     assert_eq!(enc_pt_i16, dec_pt_i16);
//     assert_eq!(enc_pt_i32, dec_pt_i32);
//     assert_eq!(enc_pt_i64, dec_pt_i64);
//     assert_eq!(enc_pt_bool, dec_pt_bool);
//     assert_eq!(enc_pt_f32, dec_pt_f32);
//     assert_eq!(enc_pt_f64, dec_pt_f64);
// }

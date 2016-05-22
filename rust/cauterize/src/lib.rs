extern crate byteorder;
use std::io::{Write, Read, Cursor};
use byteorder::{ReadBytesExt, WriteBytesExt, LittleEndian};

#[derive(Debug)]
pub enum CautResult {
    TakeError,
    PutError,
}

pub struct Decoder {
    pub csr: Cursor<Vec<u8>>,
}
impl Decoder {
    pub fn new(buf: Vec<u8>) -> Self {
        Decoder { csr: Cursor::new(buf) }
    }

    pub fn take_u8(&mut self) -> Result<u8, CautResult> {
        let buf: &mut [u8] = &mut [0];
        match self.csr.read(buf) {
            Result::Ok(_) => Result::Ok(buf[0]),
            Result::Err(_) => Result::Err(CautResult::TakeError),
        }
    }

    pub fn take_i8(&mut self) -> Result<i8, CautResult> {
        let buf: &mut [u8] = &mut [0];
        match self.csr.read(buf) {
            Result::Ok(_) => Result::Ok(buf[0] as i8),
            Result::Err(_) => Result::Err(CautResult::TakeError),
        }
    }

    pub fn take_u16(&mut self) -> Result<u16, CautResult> {
        match self.csr.read_u16::<LittleEndian>() {
            Result::Ok(val) => Result::Ok(val),
            Result::Err(_) => Result::Err(CautResult::TakeError),
        }
    }

    pub fn take_i16(&mut self) -> Result<i16, CautResult> {
        match self.csr.read_i16::<LittleEndian>() {
            Result::Ok(val) => Result::Ok(val),
            Result::Err(_) => Result::Err(CautResult::TakeError),
        }
    }

    pub fn take_u32(&mut self) -> Result<u32, CautResult> {
        match self.csr.read_u32::<LittleEndian>() {
            Result::Ok(val) => Result::Ok(val),
            Result::Err(_) => Result::Err(CautResult::TakeError),
        }
    }

    pub fn take_i32(&mut self) -> Result<i32, CautResult> {
        match self.csr.read_i32::<LittleEndian>() {
            Result::Ok(val) => Result::Ok(val),
            Result::Err(_) => Result::Err(CautResult::TakeError),
        }
    }

    pub fn take_u64(&mut self) -> Result<u64, CautResult> {
        match self.csr.read_u64::<LittleEndian>() {
            Result::Ok(val) => Result::Ok(val),
            Result::Err(_) => Result::Err(CautResult::TakeError),
        }
    }

    pub fn take_i64(&mut self) -> Result<i64, CautResult> {
        match self.csr.read_i64::<LittleEndian>() {
            Result::Ok(val) => Result::Ok(val),
            Result::Err(_) => Result::Err(CautResult::TakeError),
        }
    }

    pub fn take_f32(&mut self) -> Result<f32, CautResult> {
        match self.csr.read_f32::<LittleEndian>() {
            Result::Ok(val) => Result::Ok(val),
            Result::Err(_) => Result::Err(CautResult::TakeError),
        }
    }

    pub fn take_f64(&mut self) -> Result<f64, CautResult> {
        match self.csr.read_f64::<LittleEndian>() {
            Result::Ok(val) => Result::Ok(val),
            Result::Err(_) => Result::Err(CautResult::TakeError),
        }
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

    pub fn put_u8(&mut self, val: u8) -> Result<(), CautResult> {
        let buf: &[u8] = &[val];
        match self.csr.write(buf) {
            Result::Ok(1) => Result::Ok(()),
            _ => Result::Err(CautResult::PutError),
        }
    }

    pub fn put_i8(&mut self, val: i8) -> Result<(), CautResult> {
        let buf: &[u8] = &[val as u8];
        match self.csr.write(buf) {
            Result::Ok(1) => Result::Ok(()),
            _ => Result::Err(CautResult::PutError),
        }
    }

    pub fn put_u16(&mut self, val: u16) -> Result<(), CautResult> {
        match self.csr.write_u16::<LittleEndian>(val) {
            Result::Ok(()) => Result::Ok(()),
            _ => Result::Err(CautResult::PutError),
        }
    }

    pub fn put_i16(&mut self, val: i16) -> Result<(), CautResult> {
        match self.csr.write_i16::<LittleEndian>(val) {
            Result::Ok(()) => Result::Ok(()),
            _ => Result::Err(CautResult::PutError),
        }
    }

    pub fn put_u32(&mut self, val: u32) -> Result<(), CautResult> {
        match self.csr.write_u32::<LittleEndian>(val) {
            Result::Ok(()) => Result::Ok(()),
            _ => Result::Err(CautResult::PutError),
        }
    }

    pub fn put_i32(&mut self, val: i32) -> Result<(), CautResult> {
        match self.csr.write_i32::<LittleEndian>(val) {
            Result::Ok(()) => Result::Ok(()),
            _ => Result::Err(CautResult::PutError),
        }
    }

    pub fn put_u64(&mut self, val: u64) -> Result<(), CautResult> {
        match self.csr.write_u64::<LittleEndian>(val) {
            Result::Ok(()) => Result::Ok(()),
            _ => Result::Err(CautResult::PutError),
        }
    }

    pub fn put_i64(&mut self, val: i64) -> Result<(), CautResult> {
        match self.csr.write_i64::<LittleEndian>(val) {
            Result::Ok(()) => Result::Ok(()),
            _ => Result::Err(CautResult::PutError),
        }
    }

    pub fn put_f32(&mut self, val: f32) -> Result<(), CautResult> {
        match self.csr.write_f32::<LittleEndian>(val) {
            Result::Ok(()) => Result::Ok(()),
            _ => Result::Err(CautResult::PutError),
        }
    }

    pub fn put_f64(&mut self, val: f64) -> Result<(), CautResult> {
        match self.csr.write_f64::<LittleEndian>(val) {
            Result::Ok(()) => Result::Ok(()),
            _ => Result::Err(CautResult::PutError),
        }
    }
}


pub trait Cauterize: 'static + Sized {
    fn decode(&mut self, &mut Decoder) -> Result<(), CautResult>;
    fn encode(&self, &mut Encoder) -> Result<(), CautResult>;
}



#[test]
fn it_works() {
    let buf: Vec<u8> = Vec::new();
    let mut ectx = Encoder::new(buf);
    let val_u8 = 1 as u8;
    let val_i8 = -1 as i8;
    let val_u16 = 2 as u16;
    let val_i16 = -2 as i16;
    let val_u32 = 3 as u32;
    let val_i32 = -3 as i32;
    let val_u64 = 4 as u64;
    let val_i64 = -4 as i64;
    let val_f32 = 5.0 as f32;
    let val_f64 = -5.0 as f64;

    ectx.put_u8(val_u8).unwrap();
    ectx.put_i8(val_i8).unwrap();
    ectx.put_u16(val_u16).unwrap();
    ectx.put_i16(val_i16).unwrap();
    ectx.put_u32(val_u32).unwrap();
    ectx.put_i32(val_i32).unwrap();
    ectx.put_u64(val_u64).unwrap();
    ectx.put_i64(val_i64).unwrap();
    ectx.put_f32(val_f32).unwrap();
    ectx.put_f64(val_f64).unwrap();

    let buf = ectx.consume();
    let mut dctx = Decoder::new(buf);
    assert!(dctx.take_u8().unwrap() == val_u8);
    assert!(dctx.take_i8().unwrap() == val_i8);
    assert!(dctx.take_u16().unwrap() == val_u16);
    assert!(dctx.take_i16().unwrap() == val_i16);
    assert!(dctx.take_u32().unwrap() == val_u32);
    assert!(dctx.take_i32().unwrap() == val_i32);
    assert!(dctx.take_u64().unwrap() == val_u64);
    assert!(dctx.take_i64().unwrap() == val_i64);
    assert!(dctx.take_f32().unwrap() == val_f32);
    assert!(dctx.take_f64().unwrap() == val_f64);
}

#![feature(slice_patterns)]
use std::io;
use std::io::{Read, Write};
extern crate aey;
#[allow(unused_imports)]
use aey::*;

const SCHEMA_LEN_WORD_SIZE: usize = 1;
const SCHEMA_FP_WORD_SIZE: usize = 1;

#[derive(Debug,Clone)]
struct Header {
    len: usize,
    fingerprint: [u8; SCHEMA_FP_WORD_SIZE],
}

impl Header {
    fn read(stream: &mut Read) -> Result<Header, (&'static str)> {
        let mut len: usize = 0;
        let up: *mut usize = &mut len;
        let bp: *mut u8 = up as *mut u8;
        let mut buf = unsafe { std::slice::from_raw_parts_mut(bp, SCHEMA_LEN_WORD_SIZE) };
        match stream.read_exact(&mut buf) {
            Ok(_) => (),
            Err(_) => return Err("Cound not read len"),
        };
        let len = usize::from_le(len);

        let mut fingerprint = [0u8; SCHEMA_FP_WORD_SIZE];
        match stream.read_exact(&mut fingerprint) {
            Ok(_) => (),
            Err(_) => return Err("Cound not read fingerprint"),
        };
        Ok(Header {
            len: len,
            fingerprint: fingerprint,
        })
    }


    fn write(&self, stream: &mut Write) -> Result<(), &'static str> {
        let len = usize::to_le(self.len);
        let up: *const usize = &len;
        let bp: *const u8 = up as *const u8;
        let buf = unsafe { std::slice::from_raw_parts(bp, SCHEMA_LEN_WORD_SIZE) };
        match stream.write_all(&buf) {
            Ok(_) => (),
            Err(_) => return Err("Cound not write len"),
        };
        match stream.write_all(&self.fingerprint) {
            Ok(_) => (),
            Err(_) => return Err("Cound not write fingerprint"),
        };
        Ok(())
    }
}

#[derive(Debug,Clone)]
struct Message {
    header: Header,
    payload: Vec<u8>,
}

impl Message {
    fn read(stream: &mut Read) -> Result<Message, &'static str> {
        let header = try!(Header::read(stream));
        let mut payload = Vec::new();
        let mut chunk = stream.take(header.len as u64);
        match chunk.read_to_end(&mut payload) {
            Ok(_) => (),
            Err(_) => return Err("Cound not read payload"),
        };
        let msg = Message {
            header: header,
            payload: payload,
        };
        Ok(msg)
    }

    fn write(&self, stream: &mut Write) -> Result<(), &'static str> {
        try!(self.header.write(stream));
        match stream.write_all(&self.payload) {
            Ok(_) => (),
            Err(_) => return Err("Cound not write payload"),
        };
        Ok(())
    }
}

fn decode_then_encode(message: &Message) -> Result<Message, &'static str> {
    let mut dctx = aey::Decoder::new(message.payload.clone());

    let ebuf = Vec::new();
    let mut ectx = aey::Encoder::new(ebuf);

    match message.header.fingerprint {
        [0xd2] => {
            let a = match A::decode(&mut dctx) {
                Ok(a) => a,
                Err(_) => return Err("Could not decode type A"),
            };
            match a.encode(&mut ectx) {
                Ok(_) => (),
                Err(_) => return Err("Could not encode type A"),
            };
            let ebuf = ectx.consume();
            let message = Message {
                header: Header {
                    len: ebuf.len(),
                    fingerprint: [A::FINGERPRINT[0]],
                },
                payload: ebuf,
            };
            Ok(message)
        }
        [_] => Err("Fingerprint not recognized"),
    }
}



fn main() {
    let decoded_message = Message::read(&mut io::stdin()).unwrap();
    let encoded_message = decode_then_encode(&decoded_message).unwrap();
    encoded_message.write(&mut io::stdout()).unwrap()
}

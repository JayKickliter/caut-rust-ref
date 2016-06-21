{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Cauterize.RustRef.Tester
  ( genTester
  ) where

import qualified Cauterize.Hash            as H
import qualified Cauterize.RustRef.Util    as U
import qualified Cauterize.Specification   as S
import           Data.String.Interpolation
import qualified Data.Text                 as T


genFingerprint :: Integer -> H.Hash -> T.Text
genFingerprint fpLen f  = T.intercalate ", " bytes
  where
    bytes = map (T.pack . U.showByte) (take (fromIntegral fpLen) (H.hashToBytes f))

makeTypeList :: Integer -> [S.Type] -> [(T.Text,T.Text)]
makeTypeList fpLen ts = zip typeNames fingerPrints
  where
    typeNames = map (T.pack . U.cautTypeToRustType . S.typeName) ts
    fingerPrints = map ((genFingerprint fpLen) . S.typeFingerprint) ts

genMatchArm :: (T.Text,T.Text) -> T.Text
genMatchArm (typeName, pattern) =
  [str|[$pattern$] => {
           let a = match $typeName$::decode(&mut dctx) {
               Ok(a) => a,
               Err(_) => return Err("Could not decode type $typeName$"),
           };
           match a.encode(&mut ectx) {
               Ok(_) => (),
               Err(_) => return Err("Could not encode type $typeName$"),
           };
           let ebuf = ectx.consume();
           let message = Message {
               header: Header {
                   len: ebuf.len(),
                   fingerprint: [$pattern$],
               },
               payload: ebuf,
           };
           Ok(message)
       }
       |]

genTester :: S.Specification -> T.Text
genTester S.Specification {..} = [str|
  ##![feature(slice_patterns)]
  use std::io;
  use std::io::{Read, Write};
  extern crate $specName$;
  ##[allow(unused_imports)]
  use $specName$::*;
  use $specName$::cauterize::Cauterize;
  extern crate byteorder;
  use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};

  const FP_SIZE: usize = $:fpLen$;

  ##[derive(Debug,Clone)]
  struct Header {
      len: usize,
      fingerprint: [u8; FP_SIZE],
  }

  impl Header {
      fn read(stream: &mut Read) -> Result<Header, (&'static str)> {
          // Read length tag from message
          let len = match stream.read_$lenType$::<LittleEndian>() {
              Ok(l) => l as usize,
              Err(_) => return Err("Could not read length tag"),
          };

          // Read finger print size, or how many bytes we will use
          // to pattern match on to decide which type we're decoding
          let mut fingerprint = [0u8; FP_SIZE];
          match stream.read_exact(&mut fingerprint) {
              Ok(_) => (),
              Err(_) => return Err("Cound not read fingerprint"),
          };

          // Construct the header
          Ok(Header {
              len: len,
              fingerprint: fingerprint,
          })
      }


      fn write(&self, stream: &mut Write) -> Result<(), &'static str> {
          // Write length tag
          match stream.write_$lenType$::<LittleEndian>(self.len as $lenType$) {
              Ok(_) => (),
              Err(_) => return Err("Cound not write length tag"),
          };

          // Write fingerprint bytes
          match stream.write_all(&self.fingerprint) {
              Ok(_) => (),
              Err(_) => return Err("Cound not write fingerprint"),
          };
          Ok(())
      }
  }

  ##[derive(Debug,Clone)]
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
      let mut dctx = cauterize::Decoder::new(message.payload.clone());

      let ebuf = Vec::new();
      let mut ectx = cauterize::Encoder::new(ebuf);

      match message.header.fingerprint {
          #t in typeList:$    genMatchArm t$#
          _ => Err("Fingerprint not recognized"),
      }
  }



  fn main() {
      let decoded_message = Message::read(&mut io::stdin()).unwrap();
      let encoded_message = decode_then_encode(&decoded_message).unwrap();
      encoded_message.write(&mut io::stdout()).unwrap()
  }
  |]
  where
    typeList = makeTypeList fpLen specTypes
    fpLen = specTypeLength
    lenType = T.pack $ U.cautTagToRustType specLengthTag

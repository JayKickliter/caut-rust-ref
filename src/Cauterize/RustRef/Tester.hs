{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Cauterize.RustRef.Tester
  ( genTester
  ) where

import qualified Cauterize.CommonTypes     as C
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
           let mut ebuf = vec![0u8; message.payload.len()];
           let written = {
               let mut dctx = cauterize::Decoder::new(&message.payload);
               let mut ectx = cauterize::Encoder::new(&mut ebuf);
               let a = $typeName$::decode(&mut dctx)?;
               a.encode(&mut ectx)?;
               ectx.consume()
           };
           ebuf.truncate(written);
           let message = Message {
               header: Header {
                   len: written,
                   fingerprint: [$pattern$],
               },
               payload: ebuf,
           };
           Ok(message)
       }
       |]

genTester :: S.Specification -> T.Text
genTester S.Specification {..} = [str|
  ##![allow(unused_imports)]
  use std::io::{Read, Write};
  extern crate $specName$;
  ##[allow(unused_imports)]
  use $specName$::*;
  use $specName$::cauterize::{Cauterize, Decoder, Encoder};
  extern crate byteorder;
  use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};

  const FP_SIZE: usize = $:fpLen$;

  ##[derive(Debug)]
  enum TestError {
      Io(::std::io::Error),
      Cauterize(cauterize::Error),
      Fingerprint,
  }

  impl From<::std::io::Error> for TestError {
       fn from(err: ::std::io::Error) -> TestError {
          TestError::Io(err)
      }
  }

  impl From<cauterize::Error> for TestError {
       fn from(err: cauterize::Error) -> TestError {
          TestError::Cauterize(err)
      }
  }

  ##[derive(Debug,Clone)]
  struct Header {
      len: usize,
      fingerprint: [u8; FP_SIZE],
  }

  impl Header {
      fn read(stream: &mut Read) -> Result<Header, TestError> {
          let len = stream.$readLenTag$?;
          let mut fingerprint = [0u8; FP_SIZE];
          stream.read_exact(&mut fingerprint)?;
          Ok(Header {
              len: len as usize,
              fingerprint: fingerprint,
          })
      }


      fn write(&self, stream: &mut Write) -> Result<(), TestError> {
          stream.$writeLenTag$?;
          stream.write_all(&self.fingerprint)?;
          Ok(())
      }
  }

  ##[derive(Debug,Clone)]
  struct Message {
      header: Header,
      payload: Vec<u8>,
  }

  impl Message {
      fn read(stream: &mut Read) -> Result<Message, TestError> {
          let header = Header::read(stream)?;
          let mut payload = Vec::new();
          let mut chunk = stream.take(header.len as u64);
          chunk.read_to_end(&mut payload)?;
          let msg = Message {
              header: header,
              payload: payload,
          };
          Ok(msg)
      }

      fn write(&self, stream: &mut Write) -> Result<(), TestError> {
          self.header.write(stream)?;
          stream.write_all(&self.payload)?;
          Ok(())
      }
  }

  fn decode_then_encode(message: &Message) -> Result<Message, TestError> {
      match message.header.fingerprint {
          #t in typeList:$    genMatchArm t$#
          _ => Err(TestError::Fingerprint),
      }
  }

  fn tester() {
      let decoded_message = Message::read(&mut ::std::io::stdin()).expect("Failed to read message from stdin.");
      let encoded_message = decode_then_encode(&decoded_message).expect("Failed to dec/enc message.");
      encoded_message.write(&mut ::std::io::stdout()).expect("Failed to write encoded message to stdout.");
  }

  fn main() {
      let t = ::std::thread::Builder::new()
          .stack_size(1024 * 1024 * 32)
          .spawn(tester)
          .expect("Failed to create a new thread.");
      t.join().expect("Failed to joing tester thread.");
  }
  |]
  where
    typeList = makeTypeList fpLen specTypes
    fpLen = specTypeLength
    readLenTag = case specLengthTag of
                   C.T1 -> "read_u8()"
                   C.T2 -> "read_u16::<LittleEndian>()"
                   C.T4 -> "read_u32::<LittleEndian>()"
                   C.T8 -> "read_u64::<LittleEndian>()"
    writeLenTag = case specLengthTag of
                   C.T1 -> "write_u8(self.len as u8)"
                   C.T2 -> "write_u16::<LittleEndian>(self.len as u16)"
                   C.T4 -> "write_u32::<LittleEndian>(self.len as u32)"
                   C.T8 -> "write_u64::<LittleEndian>(self.len as u64)"

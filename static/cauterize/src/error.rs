#[derive(Debug)]
pub enum Error {
    Encode,
    Decode,
    InvalidTag,
    InvalidValue,
    ElementCount,
    OutOfRange,
}

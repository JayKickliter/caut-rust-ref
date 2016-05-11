pub mod rust_test {
    #[derive(Debug)]
    pub struct Unsigned8(u8);

    // Not declaring range SomeRange.
    // Range type not yet implemented.

    #[derive(Debug)]
    pub enum PrimitiveTest {
        U8(u8), // caut index = 0
        U16(u16), // caut index = 1
        U32(u32), // caut index = 2
        U64(u64), // caut index = 3
        S8(i8), // caut index = 4
        S16(i16), // caut index = 5
        S32(i32), // caut index = 6
        S64(i64), // caut index = 7
        Bool(bool), // caut index = 8
        F32(f32), // caut index = 9
        F64(f64), // caut index = 10
    }

    #[derive(Debug)]
    pub struct Number64(i64);

    #[derive(Debug)]
    pub struct SomeArray([Number64; 8]);

    #[derive(Debug)]
    pub struct SomeVector(Vec<Number64>);

    #[derive(Debug)]
    pub enum FieldEnum {
        Somearray, // caut idx = 0
        Somevector, // caut idx = 1
        Arecord, // caut idx = 2
    }

    #[derive(Debug)]
    pub struct Header(Vec<FieldEnum>);

    #[derive(Debug)]
    pub enum Color {
        Red, // caut idx = 0
        Green, // caut idx = 1
        Blue, // caut idx = 2
    }

    #[derive(Debug)]
    pub struct CRecord {
        pub a: i8, // caut index = 0
        pub b: i8, // caut index = 1
    }

    #[derive(Debug)]
    pub struct BRecord {
        pub a: i8, // caut index = 0
        pub d: CRecord, // caut index = 1
    }

    #[derive(Debug)]
    pub struct ARecord {
        pub z: SomeVector, // caut index = 0
        pub a: i8, // caut index = 1
        pub d: BRecord, // caut index = 2
    }

    #[derive(Debug)]
    pub enum AUnion {
        A(ARecord), // caut index = 0
        B(BRecord), // caut index = 1
        C(i8), // caut index = 2
        D(Number64), // caut index = 3
        E, // caut idx = 4
    }

    #[derive(Debug)]
    pub struct ACombination {
        pub a: Option<Number64>, // caut index = 0
        pub b: Option<i8>, // caut index = 1
        pub c: Option<AUnion>, // caut index = 2
        pub d: Option<()>, // caut idx = 3
    }
}

pub mod rust_test {
    #[derive(Debug)]
    pub struct Unsigned8(u8);

    #[derive(Debug)]
    // Range type not yet implemented.
    // Not declaring range SomeRange

    #[derive(Debug)]
    pub enum PrimitiveTest {
        U8(u8), // 0
        U16(u16), // 1
        U32(u32), // 2
        U64(u64), // 3
        S8(i8), // 4
        S16(i16), // 5
        S32(i32), // 6
        S64(i64), // 7
        Bool(bool), // 8
        F32(f32), // 9
        F64(f64), // 10
    }

    #[derive(Debug)]
    pub struct Number64(i64);

    #[derive(Debug)]
    pub struct SomeArray([Number64; 8]);

    #[derive(Debug)]
    pub struct SomeVector(Vec<Number64>);

    #[derive(Debug)]
    pub enum FieldEnum {
        Somearray, // 0
        Somevector, // 1
        Arecord, // 2
    }

    #[derive(Debug)]
    pub struct Header(Vec<FieldEnum>);

    #[derive(Debug)]
    pub enum Color {
        Red, // 0
        Green, // 1
        Blue, // 2
    }

    #[derive(Debug)]
    pub struct CRecord {
        pub a: i8, // 0
        pub b: i8, // 1
    }

    #[derive(Debug)]
    pub struct BRecord {
        pub a: i8, // 0
        pub d: CRecord, // 1
    }

    #[derive(Debug)]
    pub struct ARecord {
        pub z: SomeVector, // 0
        pub a: i8, // 1
        pub d: BRecord, // 2
    }

    #[derive(Debug)]
    pub enum AUnion {
        A(ARecord), // 0
        B(BRecord), // 1
        C(i8), // 2
        D(Number64), // 3
        E, // 4
    }

    #[derive(Debug)]
    pub struct ACombination {
        pub a: Option<Number64>, // 0
        pub b: Option<i8>, // 1
        pub c: Option<AUnion>, // 2
        pub d: Option<()>, // 3
    }

}

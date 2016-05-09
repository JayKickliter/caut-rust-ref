mod rust_test {
    #[derive(Debug)]
    pub struct Unsigned8(u8);

    #[derive(Debug)]
    pub enum Someenum {
        // fields
    }

    #[derive(Debug)]
    pub enum Primitivetest {
        u8(u8), // caut index = 0
        u16(u16), // caut index = 1
        u32(u32), // caut index = 2
        u64(u64), // caut index = 3
        s8(i8), // caut index = 4
        s16(i16), // caut index = 5
        s32(i32), // caut index = 6
        s64(i64), // caut index = 7
        bool(bool), // caut index = 8
        f32(f32), // caut index = 9
        f64(f64), // caut index = 10
    }

    #[derive(Debug)]
    pub struct Number64(i64);

    #[derive(Debug)]
    pub struct Somearray([Number64; 8]);

    #[derive(Debug)]
    pub struct Somevector(Vec<Number64>);

    #[derive(Debug)]
    pub enum FieldEnum {
        // fields
    }

    #[derive(Debug)]
    pub struct Header(Vec<FieldEnum>);

    #[derive(Debug)]
    pub struct Crecord {
        pub a: i8, // caut index = 0
        pub b: i8, // caut index = 1
    }

    #[derive(Debug)]
    pub struct Brecord {
        pub a: i8, // caut index = 0
        pub d: Crecord, // caut index = 1
    }

    #[derive(Debug)]
    pub struct Arecord {
        pub z: Somevector, // caut index = 0
        pub a: i8, // caut index = 1
        pub d: Brecord, // caut index = 2
    }

    #[derive(Debug)]
    pub enum AUnion {
        a(Arecord), // caut index = 0
        b(Brecord), // caut index = 1
        c(i8), // caut index = 2
        d(Number64), // caut index = 3
        e, // caut idx = 4
    }
}

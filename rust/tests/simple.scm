(name "simple")
(version "0.0.1")
(type number_64 synonym s64)
(type unsigned_8 synonym u8)
(type some_array array number_64 8)
(type some_vector vector number_64 8)
(type a_record record
        (fields
          (field z some_vector)
          (field a s8)
          (field d b_record)))
(type b_record record
        (fields
          (field a s8)
          (field d c_record)))
(type c_record record
        (fields
          (field a s8)
          (field b s8)))
(type a_union union
       (fields
         (field a a_record)
         (field b b_record)
         (field c s8)
         (field d number_64)
         (empty e)))
(type a_combination combination
             (fields
               (field a number_64)
               (field b s8)
               (field c a_union)
               (empty d)))
(type color enumeration
      (values
        red
        green
        blue))
(type primitive_test union
      (fields
        (field u8 u8)
        (field u16 u16)
        (field u32 u32)
        (field u64 u64)
        (field s8 s8)
        (field s16 s16)
        (field s32 s32)
        (field s64 s64)
        (field bool bool)
        (field f32 f32)
        (field f64 f64)))
(type some_range range 1000 1010)
(type field_enum enumeration
      (values
        somearray
        somevector
        arecord))
(type header vector field_enum 4)

(name "simple")
(version "0.0.1")
(fingerprint 0169f2a4cc991babfa6afc7b48b0f2c5b15640e6)
(size 1 80)
(depth 6)
(typelength 2)
(lengthtag t1)
(type 
  unsigned_8
  synonym
  (fingerprint 50f43a037b64020378c7e6f2aeb3c2854e4cdf99)
  (size 1 1)
  (depth 2)
  u8)
(type 
  some_range
  range
  (fingerprint fcf6c28dc7717d6180b5b980f6c7e3a3c7221550)
  (size 1 1)
  (depth 1)
  1000
  1010
  t1
  u16)
(type 
  primitive_test
  union
  (fingerprint 8e6c1c207920c0611417be800c91f231eac5b4f1)
  (size 2 9)
  (depth 2)
  t1
  (fields 
    (field u8 0 u8)
    (field u16 1 u16)
    (field u32 2 u32)
    (field u64 3 u64)
    (field s8 4 s8)
    (field s16 5 s16)
    (field s32 6 s32)
    (field s64 7 s64)
    (field bool 8 bool)
    (field f32 9 f32)
    (field f64 10 f64)))
(type 
  number_64
  synonym
  (fingerprint 1ca3f917f77b9d142b34478f4c9cfb047d9f6370)
  (size 8 8)
  (depth 2)
  s64)
(type 
  some_array
  array
  (fingerprint 5f44b93b749f2f6008204715a912fece747833fe)
  (size 64 64)
  (depth 3)
  number_64
  8)
(type 
  some_vector
  vector
  (fingerprint 98da3c69e1297e1d2156c0cbaff8a2d2e0bd68b4)
  (size 1 65)
  (depth 3)
  number_64
  8
  t1)
(type 
  field_enum
  enumeration
  (fingerprint 49d934d221faf807f218f6a50a648e65be1acc8a)
  (size 1 1)
  (depth 1)
  t1
  (values (value somearray 0) (value somevector 1) (value arecord 2)))
(type 
  header
  vector
  (fingerprint a93f7513a7ddb2be400141d2b7bfe6cb4b8f369f)
  (size 1 5)
  (depth 2)
  field_enum
  4
  t1)
(type 
  color
  enumeration
  (fingerprint f95edde4f1cbdd7471ed42a4b70ca4f188523738)
  (size 1 1)
  (depth 1)
  t1
  (values (value red 0) (value green 1) (value blue 2)))
(type 
  c_record
  record
  (fingerprint 550501afb46b1b5006307dc9c66318859354e133)
  (size 2 2)
  (depth 2)
  (fields (field a 0 s8) (field b 1 s8)))
(type 
  b_record
  record
  (fingerprint d21c39a58245920cc9198d4c7907eff2e81e5118)
  (size 3 3)
  (depth 3)
  (fields (field a 0 s8) (field d 1 c_record)))
(type 
  a_record
  record
  (fingerprint 252a6129ec8e4991df74a327bdc3fce7d7c46d73)
  (size 5 69)
  (depth 4)
  (fields (field z 0 some_vector) (field a 1 s8) (field d 2 b_record)))
(type 
  a_union
  union
  (fingerprint 0133189c038da7b1629d15e15daa221dd84f9900)
  (size 1 70)
  (depth 5)
  t1
  (fields 
    (field a 0 a_record)
    (field b 1 b_record)
    (field c 2 s8)
    (field d 3 number_64)
    (empty e 4)))
(type 
  a_combination
  combination
  (fingerprint 94ab0e4b372e96edcdeb9c5f82b4887d65535322)
  (size 1 80)
  (depth 6)
  t1
  (fields (field a 0 number_64) (field b 1 s8) (field c 2 a_union) (empty d 3)))

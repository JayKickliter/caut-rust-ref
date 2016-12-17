#[macro_export]
macro_rules! impl_array {
    ($name:ident, $eltype:ident, $len:expr) => (
        pub struct $name(pub [$eltype;$len]);

        impl From<[$eltype;$len]> for $name {
            fn from(a: [$eltype;$len]) -> $name {
                $name(a)
            }
        }

        impl<'a> IntoIterator for &'a $name {
            type Item = &'a $eltype;
            type IntoIter = ::std::slice::Iter<'a, $eltype>;

            fn into_iter(self) -> Self::IntoIter {
                self.as_ref().into_iter()
            }
        }

        impl<'a> IntoIterator for &'a mut $name {
            type Item = &'a mut $eltype;
            type IntoIter = ::std::slice::IterMut<'a, $eltype>;

            fn into_iter(self) -> Self::IntoIter {
                self.as_mut().iter_mut()
            }
        }

        impl AsRef<[$eltype]> for $name {
            fn as_ref(&self) -> &[$eltype] {
                &self.0
            }
        }

        impl AsMut<[$eltype]> for $name {
            fn as_mut(&mut self) -> &mut [$eltype] {
                &mut self.0
            }
        }

        impl ::std::fmt::Debug for $name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                ::std::fmt::Debug::fmt(self.as_ref(), f)
            }
        }

        impl ::std::cmp::PartialEq for $name {
            fn eq(&self, other: &$name) -> bool {
                self.as_ref() == other.as_ref()
            }
        }

        impl Clone for $name {
            fn clone(&self) -> $name {
                use std::mem;
                let mut inner: [$eltype;$len] = unsafe { ::std::mem::uninitialized()};

                for (i,s) in inner.iter_mut().zip(self.as_ref().iter()) {
                    let tmp = mem::replace(i, s.clone());
                    mem::forget(tmp);
                }
                $name(inner)
            }
        }
    )
}


#[cfg(test)]
mod tests {
    // use array;
    #[test]
    fn test_array() {
        impl_array!(TestArray, u32, 12);
        let mut test_array: TestArray = [0u32; 12].into();

        // Test AsRef, AsMut, and iterating
        for elem in test_array.as_mut().iter_mut() {
            *elem = *elem + 1;
        }

        // Test Debug
        println!("{:?}", test_array);

        // Test equality
        let test_array_cloned = test_array.clone();
        assert_eq!(test_array, test_array_cloned);
    }
}

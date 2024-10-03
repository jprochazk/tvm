use alloc::alloc::{alloc_zeroed as allocate_zeroed, dealloc as deallocate};
use core::alloc::Layout;

pub struct DynArray<T: Sized + Default + Copy> {
    base: *mut T,
    length: usize,
}

impl<T: Sized + Default + Copy> DynArray<T> {
    /// # Safety
    /// - `0` must a valid bit-pattern for `T`
    #[inline]
    pub unsafe fn new(initial_capacity: usize) -> Self {
        assert!(initial_capacity.is_power_of_two());

        let length = initial_capacity;
        let base = allocate_zeroed(Self::layout(length)).cast::<T>();
        for i in 0..length {
            base.add(i).write(T::default());
        }

        Self { base, length }
    }

    #[inline]
    pub fn offset(&self, offset: usize) -> *mut T {
        debug_assert!(offset < self.length);
        unsafe { self.base.add(offset) }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.length
    }

    #[inline]
    pub fn remaining(&self, offset: usize) -> usize {
        self.length.saturating_sub(offset)
    }

    /// Grow the stack.
    ///
    /// This invalidates any pointers to the old array.
    #[inline(never)]
    #[cold]
    pub unsafe fn grow(&mut self, additional: usize) {
        let old_length = self.length;
        let old_base = self.base;

        let new_length = (old_length + additional).next_power_of_two();
        let new_base = allocate_zeroed(Self::layout(new_length)).cast::<T>();

        core::ptr::copy_nonoverlapping(old_base, new_base, old_length);
        for i in old_length..new_length {
            new_base.add(i).write(T::default());
        }
        deallocate(old_base.cast(), Self::layout(old_length));

        self.base = new_base;
        self.length = new_length;
    }

    #[inline]
    fn layout(length: usize) -> Layout {
        unsafe { Layout::array::<T>(length).unwrap_unchecked() }
    }
}

impl<T: Sized + Default + Copy> Drop for DynArray<T> {
    #[inline]
    fn drop(&mut self) {
        unsafe { deallocate(self.base.cast(), Self::layout(self.length)) }
    }
}

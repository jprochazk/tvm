use core::alloc::Layout;

use alloc::alloc::{alloc_zeroed as allocate_zeroed, dealloc as deallocate};

pub struct DynArray<T: Sized + Copy> {
    base: *mut T,
    length: usize,
}

impl<T: Sized + Copy> DynArray<T> {
    /// # Safety
    /// - `0` must a valid bit-pattern for `T`
    #[inline]
    pub unsafe fn new() -> Self {
        unsafe {
            let length = 16 * 4096 / core::mem::size_of::<T>();

            let base = allocate_zeroed(Self::layout(length)).cast::<T>();

            Self { base, length }
        }
    }

    #[inline]
    pub fn offset(&self, offset: usize) -> *mut T {
        debug_assert!(offset < self.length);
        unsafe { self.base.add(offset) }
    }

    #[inline]
    pub fn remaining(&self, offset: usize) -> usize {
        self.length.saturating_sub(offset)
    }

    /// Grow the stack, doubling its size.
    ///
    /// Also updates `ptr` to point to the same offset in the new array.
    ///
    /// This invalidates any pointers to the old array, including `ptr`.
    pub unsafe fn grow_with_ptr(&mut self, ptr: *mut T) -> *mut T {
        let new_length = self.length * 2;
        unsafe {
            let new_base = allocate_zeroed(Self::layout(new_length)).cast::<T>();

            let new_ptr = new_base.offset(ptr.offset_from(self.base));

            core::ptr::copy_nonoverlapping(self.base, new_base, self.length);
            deallocate(self.base.cast(), Self::layout(self.length));

            self.base = new_base;
            self.length = new_length;

            new_ptr
        }
    }

    #[inline]
    fn layout(length: usize) -> Layout {
        unsafe { Layout::from_size_align_unchecked(length * core::mem::size_of::<T>(), 1) }
    }
}

impl<T: Sized + Copy> Drop for DynArray<T> {
    #[inline]
    fn drop(&mut self) {
        unsafe { deallocate(self.base.cast(), Self::layout(self.length)) }
    }
}

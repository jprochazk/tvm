use std::alloc::{dealloc, Layout};
use std::cell::Cell;
use std::marker::PhantomData;
use std::mem::align_of;
use std::ptr::{addr_of, addr_of_mut, drop_in_place, NonNull};

pub struct Rc<T: Sized + 'static> {
    ptr: NonNull<Inner<T>>,
    _p: PhantomData<Inner<T>>,
}

impl<T: Sized + 'static> Rc<T> {
    pub fn new(value: T) -> Self {
        let ptr = Box::new(Inner {
            refs: Cell::new(1),
            value,
        });
        let ptr = Box::into_raw(ptr);
        let ptr = unsafe { NonNull::new_unchecked(ptr) };
        Self {
            ptr,
            _p: PhantomData,
        }
    }

    #[inline]
    pub fn as_ref(&self) -> Ref<'_, T> {
        Ref::from_ref(self.ptr.as_ptr())
    }

    #[inline]
    pub fn refs(&self) -> usize {
        self.inner().refs.get()
    }

    #[inline]
    fn inner(&self) -> &Inner<T> {
        unsafe { self.ptr.as_ref() }
    }

    #[inline]
    fn inc_ref(&self) {
        let inner = unsafe { self.ptr.as_ref() };
        inner.refs.set(inner.refs.get() + 1);
    }

    #[inline]
    fn dec_ref(&self) {
        let inner = unsafe { self.ptr.as_ref() };
        inner.refs.set(inner.refs.get() - 1);
    }
}

impl<T> Clone for Rc<T> {
    fn clone(&self) -> Self {
        self.inc_ref();
        Self {
            ptr: self.ptr,
            _p: PhantomData,
        }
    }
}

impl<T> Drop for Rc<T> {
    fn drop(&mut self) {
        self.dec_ref();
        if self.refs() == 0 {
            unsafe {
                let inner_layout = Layout::for_value(self.ptr.as_ref());
                let inner_ptr = self.ptr.as_ptr();
                let value_ptr = addr_of_mut!((*inner_ptr).value);
                drop_in_place(value_ptr); // drop `T`
                dealloc(inner_ptr.cast(), inner_layout); // dealloc `Inner<T>`
            }
        }
    }
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Ref<'a, T: Sized + 'static> {
    ptr: *const T,
    _p: PhantomData<&'a T>,
}

impl<'a, T: Sized + 'static> Ref<'a, T> {
    #[inline]
    pub fn get(&self) -> &'a T {
        unsafe { &*self.ptr }
    }

    #[inline]
    pub fn to_owned(&self) -> Rc<T> {
        let ptr = unsafe {
            // Inner<T> { refs: Cell<usize>, value: T }
            //            ^                  ^ `self.ptr` points here
            //            | we want a pointer here

            let ptr = (self.ptr as *const u8).sub(Inner::<T>::value_offset());
            NonNull::new_unchecked(ptr as *mut Inner<T>)
        };

        let ptr = Rc {
            ptr,
            _p: PhantomData,
        };
        ptr.inc_ref();
        ptr
    }

    #[inline]
    fn from_ref(inner: *const Inner<T>) -> Self {
        unsafe {
            Ref {
                ptr: addr_of!((*inner).value),
                _p: PhantomData,
            }
        }
    }
}

#[repr(C)]
struct Inner<T: Sized + 'static> {
    refs: Cell<usize>,
    value: T,
}

impl<T: Sized + 'static> Inner<T> {
    #[inline(always)]
    const fn value_offset() -> usize {
        let base_layout = Layout::new::<Inner<()>>();
        let padding = {
            let value_align = align_of::<T>();
            // layout.padding_needed_for(align)
            let len = base_layout.size();
            let len_rounded_up =
                len.wrapping_add(value_align).wrapping_sub(1) & !value_align.wrapping_sub(1);
            len_rounded_up.wrapping_sub(len)
        };
        base_layout.size() + padding
    }
}

use std::cell::RefCell;
use std::fmt::Display;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;

pub(crate) fn num_digits(v: usize) -> usize {
  use core::iter::successors;

  successors(Some(v), |&n| (n >= 10).then_some(n / 10)).count()
}

pub(crate) trait Discard {
  type Output;
  fn discard(self) -> Self::Output;
}

impl<T, E> Discard for Result<T, E> {
  type Output = Result<(), E>;

  fn discard(self) -> Self::Output {
    match self {
      Ok(_) => Ok(()),
      Err(e) => Err(e),
    }
  }
}

impl<T> Discard for Option<T> {
  type Output = Option<()>;

  fn discard(self) -> Self::Output {
    match self {
      Some(_) => Some(()),
      None => None,
    }
  }
}

pub struct Join<Iter, Sep>
where
  Iter: Iterator,
{
  iter: RefCell<Iter>,
  peek: RefCell<Option<Option<Iter::Item>>>,
  sep: Sep,
}

impl<Iter: Iterator, Sep> Join<Iter, Sep> {
  fn next(&self) -> Option<Iter::Item> {
    match self.peek.borrow_mut().take() {
      Some(item) => item,
      None => self.iter.borrow_mut().next(),
    }
  }

  fn has_next(&self) -> bool {
    let mut iter = self.iter.borrow_mut();
    let mut peek = self.peek.borrow_mut();
    peek.get_or_insert_with(|| iter.next());
    peek.is_some()
  }
}

impl<Iter, Sep> Display for Join<Iter, Sep>
where
  Iter: Iterator,
  <Iter as Iterator>::Item: Display,
  Sep: Display,
{
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    while let Some(item) = self.next() {
      write!(f, "{item}")?;
      if self.has_next() {
        write!(f, "{}", self.sep)?;
      }
    }
    Ok(())
  }
}

pub trait JoinIter: Sized + Iterator {
  fn join<Sep>(self, sep: Sep) -> Join<Self, Sep>;
}

impl<Iter> JoinIter for Iter
where
  Iter: Sized + Iterator,
{
  fn join<Sep>(self, sep: Sep) -> Join<Self, Sep> {
    Join {
      iter: RefCell::new(self),
      peek: RefCell::new(None),
      sep,
    }
  }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct HashOf<T: Hash>(u64, PhantomData<fn(T) -> T>);

impl<T: Hash> HashOf<T> {
  pub fn new(v: &T) -> Self {
    Self(hash(v), PhantomData)
  }
}

impl<T: Hash> Hash for HashOf<T> {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    state.write_u64(self.0);
  }
}

pub fn hash(v: &impl Hash) -> u64 {
  let mut state = rustc_hash::FxHasher::default();
  v.hash(&mut state);
  state.finish()
}

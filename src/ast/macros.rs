macro_rules! syntax_node {
  (
    $name:ident<$lifetime:lifetime> {
      $(
        $variant:ident $({
          $($field:ident : $ty:ty),* $(,)?
        })?
      ),* $(,)?
    }
  ) => {
    paste::paste! {
      #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
      pub struct [<$name Id>](u32);

      impl Default for [<$name Id>] {
        fn default() -> Self {
          Self(0)
        }
      }

      impl Next for [<$name Id>] {
        fn next(&self) -> Self {
          Self(self.0 + 1)
        }
      }

      #[derive(Clone)]
      pub struct $name<$lifetime> {
        pub id: [<$name Id>],
        pub kind: [<$name Kind>]<$lifetime>,
        pub span: Span,
      }

      impl<$lifetime> $name<$lifetime> {
        pub fn new(
          id: [<$name Id>],
          kind: [<$name Kind>]<$lifetime>,
          span: Span,
        ) -> Self {
          Self {
            id,
            kind,
            span,
          }
        }
      }

      impl<$lifetime> ::core::fmt::Debug for $name<$lifetime> {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
          match &self.kind {
            $([<$name Kind>]::$variant(inner) => ::core::fmt::Debug::fmt(&*inner, f),)*
          }
        }
      }

      impl<$lifetime> $name<$lifetime> {
        $(
          pub fn [<make_ $variant:snake>](
            id: [<$name Id>],
            span: impl Into<crate::lex::Span>,
            $($($field : $ty),*)?
          ) -> Self {
            Self::new(
              id,
              [<$name Kind>]::$variant(
                [<$variant $name>]::new(
                  $($($field),*)?
                ).wrap_box()
              ),
              span.into(),
            )
          }

          pub fn [<is_ $variant:snake>](&self) -> bool {
            matches!(self.kind, [<$name Kind>]::$variant(..))
          }

          #[allow(unused_parens, unreachable_patterns)]
          pub fn [<into_ $variant:snake>](self) -> Result<[<$variant $name>]<$lifetime>, $name<$lifetime>> {
            match self.kind {
              [<$name Kind>]::$variant(inner) => Ok(inner.unwrap_box()),
              _ => Err(self),
            }
          }

          #[allow(unused_parens, unreachable_patterns)]
          pub fn [<as_ $variant:snake>](&self) -> Option<&([<$variant $name>]<$lifetime>)> {
            match &self.kind {
              [<$name Kind>]::$variant(inner) => Some(&*inner),
              _ => None,
            }
          }
        )*
      }

      #[derive(Clone, Debug)]
      pub enum [<$name Kind>]<$lifetime> {
        $(
          $variant(
            syntax_node!(@maybe_box $variant $name $lifetime $({ $($field : $ty),* })?)
          )
        ),*
      }
    }

    $(
      syntax_node!(@variant_struct $variant $name $lifetime $({ $($field : $ty),* })?);
    )*
  };

  (@maybe_box
    $variant:ident
    $name:ident
    $lifetime:lifetime
    { $($field:ident : $ty:ty),* }
  ) => {
    paste::paste! {
      Box<[<$variant $name>]<$lifetime>>
    }
  };
  (@maybe_box
    $variant:ident
    $name:ident
    $lifetime:lifetime
  ) => {
    paste::paste! {
      [<$variant $name>]<$lifetime>
    }
  };

  (@variant_struct
    $variant:ident
    $name:ident
    $lifetime:lifetime
    { $($field:ident : $ty:ty),* }
  ) => {
    paste::paste! {
      #[derive(Clone)]
      pub struct [<$variant $name>]<$lifetime> {
        _lifetime: std::marker::PhantomData<& $lifetime ()>,
        $(pub $field : $ty),*
      }

      impl<$lifetime> ::core::fmt::Debug for [<$variant $name>]<$lifetime> {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
          let mut s = f.debug_tuple(stringify!([<$variant>]));
          $(s.field(&self.$field);)*
          s.finish()
        }
      }

      impl<$lifetime> [<$variant $name>]<$lifetime> {
        pub fn new($($field : $ty),*) -> Self {
          Self {
            _lifetime: std::marker::PhantomData,
            $($field),*
          }
        }
      }

      impl<$lifetime> WrapBox for [<$variant $name>]<$lifetime> {
        type Boxed = Box<Self>;

        fn wrap_box(self) -> Self::Boxed {
          Box::new(self)
        }
      }

      impl<$lifetime> UnwrapBox for Box<[<$variant $name>]<$lifetime>> {
        type Unboxed = [<$variant $name>]<$lifetime>;

        fn unwrap_box(self) -> Self::Unboxed {
          *self
        }
      }
    }
  };

  (@variant_struct
    $variant:ident
    $name:ident
    $lifetime:lifetime
  ) => {
    paste::paste! {
      #[derive(Clone)]
      pub struct [<$variant $name>]<$lifetime>(std::marker::PhantomData<& $lifetime ()>);

      impl<$lifetime> [<$variant $name>]<$lifetime> {
        pub fn new() -> Self {
          Self(std::marker::PhantomData)
        }
      }

      impl<$lifetime> ::core::fmt::Debug for [<$variant $name>]<$lifetime> {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
          f.write_str(stringify!([<$variant>]))
        }
      }

      impl<$lifetime> WrapBox for [<$variant $name>]<$lifetime> {
        type Boxed = Self;

        fn wrap_box(self) -> Self::Boxed {
          self
        }
      }

      impl<$lifetime> UnwrapBox for [<$variant $name>]<$lifetime> {
        type Unboxed = Self;

        fn unwrap_box(self) -> Self::Unboxed {
          self
        }
      }
    }
  };
}

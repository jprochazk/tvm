#[macro_use]
mod macros;

use std::fmt::Write;

use convert_case::{Case, Casing};
use proc_macro2::{Ident, Literal, TokenStream};
use quote::{ToTokens, TokenStreamExt as _};

use crate::decl::*;

pub fn run(bytecode: &Bytecode) -> String {
    let mut buf = String::new();

    base(&mut buf, bytecode);
    operands(&mut buf, bytecode);
    assembler(&mut buf, bytecode);
    symbolic(&mut buf, bytecode);
    dispatch(&mut buf, bytecode);
    types(&mut buf, bytecode);
    p!(buf, include_str!("./support.rs"));

    buf
}

fn base(buf: &mut impl Write, bytecode: &Bytecode) {
    p!(
        buf,
        "//! Instruction encoding, decoding, disassembly, and dispatch."
    );
    q!(buf, [#![allow(dead_code, unused_variables)]]);
    nl!(buf);

    // enum
    {
        let variants = bytecode.ops.iter().enumerate().map(|(i, (name, _))| {
            let idx = Unsuffixed(i);
            let name = name.to_case(Case::Pascal).to_ident();
            q!([#name = #idx])
        });

        q!(buf, [
            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            #[repr(u8)]
            pub enum Op {
                #(#variants),*
            }
        ]);
        nl!(buf);
    }

    // `MIN`/`MAX` constants
    {
        let (min, max) = (
            bytecode.first().0.to_case(Case::Pascal).to_ident(),
            bytecode.last().0.to_case(Case::Pascal).to_ident(),
        );
        q!(buf, [
            impl Op {
                const MIN: u8 = Op::#min as u8;
                const MAX: u8 = Op::#max as u8;
            }
        ]);
        nl!(buf);
    }

    // encode/decode impl
    {
        q!(buf, [
            impl Encode for Op {
                #[inline]
                fn encode<E>(self, enc: &mut E)
                where
                    E: ?Sized + Encoder,
                {
                    enc.encode_u8(self as u8)
                }
            }

            impl Decode for Op {
                #[inline(always)]
                unsafe fn decode_unchecked(buf: &mut impl Decoder) -> Self {
                    let v = buf.decode_u8_unchecked();
                    debug_assert!(v <= Op::MAX);
                    unsafe { core::mem::transmute(v) }
                }
            }
        ]);
        nl!(buf);
    }
}

fn operands(buf: &mut impl Write, bytecode: &Bytecode) {
    let items = bytecode.ops.iter().map(|(name, operands)| {
        let name_pascal = name.to_case(Case::Pascal).to_ident();
        let operands = operands.iter().map(|(name, ty)| {
            let operand_name = name.to_ident();
            let operand_ty = ty.to_ident();
            (operand_name, operand_ty)
        });
        let decl_fields = operands.clone().map(|(name, ty)| q!([pub #name: #ty]));
        let decode_fields = operands
            .clone()
            .map(|(name, _)| q!([#name: Decode::decode_unchecked(buf)]));
        let encode_fields = operands.map(|(name, _)| q!([self.#name.encode(enc);]));

        q!([
            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            pub struct #name_pascal {
                #(#decl_fields),*
            }

            impl Decode for #name_pascal {
                #[inline(always)]
                unsafe fn decode_unchecked(buf: &mut impl Decoder) -> Self {
                    Self {
                        #(#decode_fields),*
                    }
                }
            }

            impl Encode for #name_pascal {
                #[inline]
                fn encode<E>(self, enc: &mut E)
                where
                    E: ?Sized + Encoder,
                {
                    Op::#name_pascal.encode(enc);
                    #(#encode_fields)*
                }
            }
        ])
    });

    q!(buf, [
        pub mod operands {
            #![doc = " Operand encoding and decoding"]

            use super::*;

            #(#items)*
        }
    ]);
    nl!(buf);
}

fn assembler(buf: &mut impl Write, bytecode: &Bytecode) {
    let items = bytecode.ops.iter().map(|(name, operands)| {
        let name_lower = name.to_ident();
        let name_pascal = name.to_case(Case::Pascal).to_ident();
        let params = operands.iter().map(|(name, ty)| {
            let name = name.to_ident();
            let ty = ty.to_ident();
            q!([#name: #ty])
        });
        let struct_fields = operands.iter().map(|(name, _)| {
            let name = name.to_ident();
            q!([#name])
        });

        q!([
            #[inline]
            pub fn #name_lower(#(#params),*) -> impl Encode {
                operands::#name_pascal {
                    #(#struct_fields),*
                }
            }
        ])
    });

    q!(buf, [
        pub mod asm {
            #![doc = " Bytecode assembler"]

            use super::*;

            #(#items)*
        }
    ]);
    nl!(buf);
}

fn symbolic(buf: &mut impl Write, bytecode: &Bytecode) {
    let variants = bytecode.ops.keys().map(|name| {
        let name_pascal = name.to_case(Case::Pascal).to_ident();
        q!([#name_pascal(#name_pascal)])
    });

    let decode_arms = bytecode.ops.keys().map(|name| {
        let name_pascal = name.to_case(Case::Pascal).to_ident();
        q!([Op::#name_pascal => Instruction::#name_pascal(#name_pascal::decode_unchecked(dec))])
    });

    let display_arms = bytecode.ops.iter().map(|(name, operands)| {
        let name_pascal = name.to_case(Case::Pascal).to_ident();
        let name_snake = name;
        let operands_csv = operands
            .iter()
            .map(|(name, _)| format!("{{{name}}}"))
            .collect::<Vec<_>>()
            .join(", ");
        let fmt = format!("{name_snake} {operands_csv}");
        let fields = operands.iter().map(|(name, _)| name.to_ident());
        q!([Instruction::#name_pascal(#name_pascal { #(#fields),* }) => write!(f, #fmt)])
    });

    q!(buf, [
        pub mod symbolic {
            #![doc = " Symbolic representation of bytecode.ops"]

            use super::*;
            pub use super::operands::*;

            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            pub enum Instruction {
                #(#variants),*
            }

            impl Decode for Instruction {
                unsafe fn decode_unchecked(dec: &mut impl Decoder) -> Self {
                    match Op::decode_unchecked(dec) {
                        #(#decode_arms),*
                    }
                }
            }

            impl std::fmt::Display for Instruction {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    match self {
                        #(#display_arms),*
                    }
                }
            }
        }
    ]);
    nl!(buf);
}

fn dispatch(buf: &mut impl Write, bytecode: &Bytecode) {
    let handler_trait = {
        let ops = bytecode.ops.iter().map(|(name, operands)| {
            let name_snake = quote::format_ident!("op_{}", name);
            let operands = operands.iter().map(|(name, ty)| {
                let name = name.to_ident();
                let ty = ty.to_ident();
                q!([#name: #ty])
            });
            q!([
                fn #name_snake(&self, #(#operands),*) -> Self::Result<()>;
            ])
        });

        q!([
            pub trait Handler {
                type Result<T>;

                #(#ops)*
            }
        ])
    };

    let dispatch_fn = {
        let ops = bytecode.ops.iter().map(|(name, operands)| {
            let name_pascal = name.to_case(Case::Pascal).to_ident();
            let handler_fn = quote::format_ident!("op_{}", name);
            let operand_names = operands.iter().map(|(name, _)| name.to_ident());
            let operand_args = operand_names.clone();
            q!([
                Op::#name_pascal => {
                    let #name_pascal { #(#operand_names),* } = #name_pascal::decode_unchecked(inst);
                    h.#handler_fn(#(#operand_args),*)
                }
            ])
        });

        q!([
            #[doc = " Dispatch a single instruction"]
            #[doc = " # Safety"]
            #[doc = " - `&code[*ip..]` must contain a valid bytecode instruction"]
            #[inline(always)]
            pub unsafe fn dispatch<H: Handler>(h: &H, ip: &mut Ip, code: &Code) -> H::Result<()> {
                let inst = &mut &code[*ip..];
                match Op::decode_unchecked(inst) {
                    #(#ops),*
                }
            }
        ])
    };

    q!(buf, [
        pub mod dispatch {
            #![doc = " Dispatch loop"]

            use super::*;
            use operands::*;

            type Code = [u8];
            type Ip = usize;

            #handler_trait

            #dispatch_fn
        }
    ]);
    nl!(buf);
}

fn types(buf: &mut impl Write, bytecode: &Bytecode) {
    for (name, ty) in bytecode.types.iter() {
        let Type { inner, fmt } = ty;
        let must_use = format!("unused {name}");
        let name = name.to_ident();
        let inner = inner.to_ident();
        q!(buf, [
            #[must_use = #must_use]
            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            pub struct #name(#inner);

            impl #name {
                #[inline]
                pub fn new(v: #inner) -> Self {
                    Self(v)
                }

                #[inline]
                pub fn try_new<T>(v: T) -> Option<Self>
                where
                    #inner: TryFrom<T>,
                {
                    <#inner>::try_from(v).map(#name).ok()
                }

                #[inline]
                pub fn get(self) -> #inner {
                    self.0
                }

                #[inline]
                pub fn to_index(self) -> usize {
                    self.0 as usize
                }
            }

            impl Encode for #name {
                #[inline]
                fn encode<E>(self, enc: &mut E)
                where
                    E: ?Sized + Encoder,
                {
                    self.0.encode(enc)
                }
            }

            impl Decode for #name {
                #[inline(always)]
                unsafe fn decode_unchecked(dec: &mut impl Decoder) -> Self {
                    Self(<#inner as Decode>::decode_unchecked(dec))
                }
            }

            impl std::fmt::Display for #name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, #fmt, self.0)
                }
            }
        ]);
        nl!(buf);
    }
}

struct Unsuffixed<T: ToLiteral + Copy>(T);
impl<T: ToLiteral + Copy> ToTokens for Unsuffixed<T> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append(self.0.to_literal());
    }
}

trait ToLiteral {
    fn to_literal(self) -> Literal;
}

impl ToLiteral for usize {
    fn to_literal(self) -> Literal {
        Literal::usize_unsuffixed(self)
    }
}

trait ToIdent {
    fn to_ident(&self) -> Ident;
}

impl ToIdent for String {
    fn to_ident(&self) -> Ident {
        quote::format_ident!("{self}")
    }
}
impl ToIdent for &'_ str {
    fn to_ident(&self) -> Ident {
        quote::format_ident!("{self}")
    }
}
impl ToIdent for std::borrow::Cow<'_, str> {
    fn to_ident(&self) -> Ident {
        quote::format_ident!("{self}")
    }
}

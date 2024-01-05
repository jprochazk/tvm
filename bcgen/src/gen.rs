#[macro_use]
mod macros;

use std::fmt::Write;

use convert_case::{Case, Casing};
use proc_macro2::{Ident, Literal, TokenStream};
use quote::{ToTokens, TokenStreamExt as _};

use crate::decl::*;

pub fn run(instructions: &[Instruction]) -> String {
    let mut buf = String::new();

    base(&mut buf, instructions);
    operands(&mut buf, instructions);
    assembler(&mut buf, instructions);
    symbolic(&mut buf, instructions);
    dispatch(&mut buf, instructions);
    p!(buf, include_str!("./support.rs"));

    buf
}

fn base(buf: &mut impl Write, instructions: &[Instruction]) {
    p!(
        buf,
        "//! Instruction encoding, decoding, disassembly, and dispatch."
    );
    q!(buf, [#![allow(dead_code, unused_variables)]]);
    nl!(buf);

    // enum
    {
        let variants = instructions.iter().enumerate().map(|(i, inst)| {
            let idx = Unsuffixed(i);
            let name = inst.name.to_case(Case::Pascal).to_ident();
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
            instructions[0].name.to_case(Case::Pascal).to_ident(),
            instructions[instructions.len() - 1]
                .name
                .to_case(Case::Pascal)
                .to_ident(),
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

fn operands(buf: &mut impl Write, instructions: &[Instruction]) {
    let items = instructions.iter().map(|inst| {
        let name_pascal = inst.name.to_case(Case::Pascal).to_ident();
        let operands = inst.operands.iter().map(|operand| {
            let operand_name = operand.name.to_ident();
            let operand_ty = operand.ty.to_ident();
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

fn assembler(buf: &mut impl Write, instructions: &[Instruction]) {
    let items = instructions.iter().map(|inst| {
        let name_lower = inst.name.to_case(Case::Snake).to_ident();
        let name_pascal = inst.name.to_case(Case::Pascal).to_ident();
        let params = inst.operands.iter().map(|operand| {
            let name = operand.name.to_ident();
            let ty = operand.ty.to_ident();
            q!([#name: #ty])
        });
        let struct_fields = inst.operands.iter().map(|operand| {
            let name = operand.name.to_ident();
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

fn symbolic(buf: &mut impl Write, instructions: &[Instruction]) {
    let variants = instructions.iter().map(|inst| {
        let name_pascal = inst.name.to_case(Case::Pascal).to_ident();
        q!([#name_pascal(#name_pascal)])
    });

    let decode_arms = instructions.iter().map(|inst| {
        let name_pascal = inst.name.to_case(Case::Pascal).to_ident();
        q!([Op::#name_pascal => Instruction::#name_pascal(#name_pascal::decode_unchecked(dec))])
    });

    let display_arms = instructions.iter().map(|inst| {
        let name_pascal = inst.name.to_case(Case::Pascal).to_ident();
        let name_snake = inst.name.to_case(Case::Snake);
        let operands_csv = inst
            .operands
            .iter()
            .map(|operand| format!("{{{}}}", operand.name))
            .collect::<Vec<_>>()
            .join(", ");
        let fmt = format!("{name_snake} {operands_csv}");
        let fields = inst.operands.iter().map(|operand| operand.name.to_ident());
        q!([Instruction::#name_pascal(#name_pascal { #(#fields),* }) => write!(f, #fmt)])
    });

    q!(buf, [
        pub mod symbolic {
            #![doc = " Symbolic representation of instructions"]

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

fn dispatch(buf: &mut impl Write, instructions: &[Instruction]) {
    let handler_trait = {
        let ops = instructions.iter().map(|inst| {
            let name_snake = quote::format_ident!("op_{}", inst.name.to_case(Case::Snake));
            let operands = inst.operands.iter().map(|operand| {
                let name = operand.name.to_ident();
                let ty = operand.ty.to_ident();
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
        let ops = instructions.iter().map(|inst| {
            let name_pascal = inst.name.to_case(Case::Pascal).to_ident();
            let handler_fn = quote::format_ident!("op_{}", inst.name);
            let operand_names = inst.operands.iter().map(|operand| operand.name.to_ident());
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

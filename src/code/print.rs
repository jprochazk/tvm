use std::fmt::Display;

use super::{CodeUnit, FnId, Function};
use crate::error::Location;
use crate::lex::Span;

pub struct DisplayModule<'a, 'src>(pub &'a CodeUnit, pub Option<&'src str>);
impl<'a, 'src> Display for DisplayModule<'a, 'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let DisplayModule(m, src) = self;

        writeln!(
            f,
            "{}",
            DisplayScriptFunction(None, &m.functions.main, *src)
        )?;
        for (id, fn_) in m.functions.script.iter().enumerate() {
            let id = Some(FnId(id as u16));
            writeln!(f, "{}", DisplayScriptFunction(id, fn_, *src))?;
        }
        Ok(())
    }
}

struct DisplayScriptFunction<'a, 'src>(Option<FnId>, &'a Function, Option<&'src str>);
impl<'a, 'src> Display for DisplayScriptFunction<'a, 'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self(id, fn_, src) = self;

        write!(f, "function {:?}", fn_.name)?;
        if let Some(id) = id {
            write!(f, " #{}", id.0)?;
        }
        writeln!(f, " {{")?;
        writeln!(f, "  registers: {} ({} params)", fn_.registers, fn_.params)?;
        write!(f, "  literals: ")?;
        if fn_.literals.is_empty() {
            writeln!(f, "[]")?;
        } else {
            writeln!(f, "[")?;
            for c in &fn_.literals {
                writeln!(f, "    {c:?}")?;
            }
            writeln!(f, "  ]")?;
        }
        writeln!(f, "  bytecode: ({} ops) [", fn_.bytecode.len())?;

        let mut max_len = 0;
        let mut instructions = Vec::with_capacity(fn_.bytecode.len());
        for op in fn_.bytecode.iter() {
            let s = op.to_string();
            max_len = std::cmp::max(max_len, s.len());
            instructions.push(s);
        }

        let mut prev_line_span = Span::empty();
        let mut instructions = instructions.into_iter().zip(fn_.spans.iter()).peekable();
        while let Some((instruction, span)) = instructions.next() {
            f.write_str("    ")?;
            'next: {
                if let Some(src) = src {
                    write!(f, "{instruction:max_len$}  // ")?;

                    if span.is_empty() {
                        break 'next;
                    }

                    let loc = Location::from_source_span(src, span);
                    if prev_line_span == loc.line_span() {
                        break 'next;
                    }
                    prev_line_span = loc.line_span();

                    f.write_str(src[loc.line_span()].trim())?;
                } else {
                    write!(f, "{instruction}")?;
                }
            }
            if instructions.peek().is_some() {
                f.write_str("\n")?;
            }
        }
        writeln!(f, "  ")?;
        writeln!(f, "  ]")?;
        writeln!(f, "}}")
    }
}

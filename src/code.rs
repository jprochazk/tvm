use crate::hir::Hir;

mod compiler;
pub mod op;

/*

// Module is the chosen unit of compilation.
// Each file/snippet is a module.
Module {
    fn_table: FnTable,
}

// Functions are dispatched from the fn table via ID,
// which is an offset into the `fns` array.
FnTable {
    fns: Vec<FnTableEntry>,
    relocs: Relocs,
}


FnTableEntry {
    // Defined within a script, though not necessarily _this_ script,
    // it may have been imported.
    Script(Function),

    // Defined externally. Also may have been imported.
    // All extern function slots are _reserved_, and must be filled
    // in by calling `link` on the `Module` with the appropriate symbols,
    // see `Relocs` below
    Host(ExternFunction),
}

// In the future, an `import` statement which imports functions
// will cause those functions to be copied into this module's
// function table, so that they may be dispatched identically
// to functions defined within that module.
// The reason to even keep modules separate at all is to allow
// compiling and caching them separately.

// From wikipedia: (https://en.wikipedia.org/wiki/Relocation_(computing))
// Relocation is the process of assigning load addresses for position-dependent
// code and data of a program and adjusting the code and data to reflect the assigned addresses.
//
// The relevant part here is "adjusting ... data to reflect the assigned addresses".
// The reserved `Host` fn table slots are filled in with actual Rust functions at link time.
Relocs {
    // Mapping of extern symbol name to fn id used to fill in the `FnTable` during linking.
    fns: BTreeMap<String, FnId>,
}

// A script-defined function.
Function {
    // always present even if not being used
    // it's safe to leave this as an empty vec
    // if the function does not use any captures
    // and it removes the need for a branch
    // in the call instruction
    captures: Vec<Value>,
}

// An externally defined function.
ExternFunction {
    // ... a boxed function pointer
}

// There are two call instructions:
// - static call
//     - used when the function has no captures and is referenced directly via name
// - value call
//     - used when the function is held in a variable/parameter
//     - this is also the only way to call closures

// Example of static call:
    fn foo() {}
    foo();
// Turns into:
    fn_table: [foo]
    bytecode:
        call 0, r0

// Example of value call:
    fn foo() {}
    let bar = foo;
    bar();
// Turns into:
    fn_table: [foo]
    bytecode:
        load_fn 0, r0
        call r0, r1

// Methods are just functions. They just happen to accept
// some `T` as the first argument.

// Example of method call:
    type T;
    fn T.foo() {}
    T().foo();
// Becomes:
    fn_table: [T.cons, T.foo]
    bytecode:
        call 0, r0
        call 1, r0, r0

// The `call` instruction now accepts an extra operand
// pointing to the first argument, which in this case
// is the result of the call to the constructor for `T`.
*/

pub fn compile(_hir: Hir<'_>) -> Module {
    todo!()
}

pub struct Module {}

/*


let src = "
    extern fn foo(s: str);
    foo("abcd");
";
let ast = syn::parse(src)?;
let hir = ty::check(&ast)?;
let obj = code::compile(&hir);

type Sender = crossbeam_channel::Sender<String>;
type Receiver = crossbeam_channel::Receiver<String>;

fn foo(ctx: &Ctx, sender: Sender) {
    ctx.register(move |vm: &Vm, s: String| -> Result<()> {
        sender.send(s);
        Ok(())
    });
}

let ctx = Ctx::new();
let (rx, tx) = crossbeam_channel::unbounded();
foo(&ctx, tx);

let module = obj.link(&ctx)?;
module.run()?;

let v = rx.recv().unwrap();
println!("{v}"); // abcd

*/

#[cfg(test)]
mod tests;

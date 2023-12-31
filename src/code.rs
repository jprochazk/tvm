use crate::hir::Hir;

mod asm;
pub mod op;

pub fn compile(_hir: Hir<'_>) -> Code {
    todo!()
}

pub struct Code {}

impl Code {
    pub fn link() -> Module {
        todo!()
    }
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

#[hebi::module]
fn foo(ctx: &Ctx, sender: Sender) -> Result<()> {
    ctx.register(move |vm: &Vm, s: String| -> Result<()> {
        sender.send(s);
        Ok(())
    });
}

let ctx = Ctx::new();
let (rx, tx) = crossbeam_channel::unbounded();
ctx.register(foo(tx));

let module = obj.link(&ctx)?;
module.run()?;

let v = rx.recv().unwrap();
println!("{v}"); // abcd

*/

#[cfg(test)]
mod tests;

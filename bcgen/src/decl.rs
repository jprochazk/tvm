#[derive(Debug)]
pub struct Instruction {
    /// Name of this instruction.
    pub name: String,

    /// Operands encoded into the instruction stream.
    pub operands: Vec<Operand>,
}

#[derive(Debug)]
pub struct Operand {
    pub name: String,
    pub ty: String,
}

pub fn parse(s: &str) -> Vec<Instruction> {
    // TODO: check for duplicates

    let mut instructions = Vec::new();

    let mut lines = s
        .lines()
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
        .enumerate();

    // parse the following format:
    //   def NAME
    //     NAME: TYPE
    //     NAME: TYPE
    //   end
    'def: while let Some((i, line)) = lines.next() {
        if !line.starts_with("def") {
            continue;
        }

        let Some((_, name)) = line.split_once(' ') else {
            eprintln!("syntax error: invalid def on line {i}:\n  {line}");
            continue;
        };
        let name = name.trim().to_string();

        let mut operands = Vec::new();
        for (i, line) in lines.by_ref() {
            if line.starts_with("end") {
                break;
            }

            let Some((name, ty)) = line.split_once(':') else {
                eprintln!("syntax error: invalid operand for def {name:?} on line {i}:\n  {line}");
                continue 'def;
            };
            let name = name.trim().to_string();
            let ty = ty.trim().to_string();

            operands.push(Operand { name, ty });
        }

        instructions.push(Instruction { name, operands });
    }

    instructions
}

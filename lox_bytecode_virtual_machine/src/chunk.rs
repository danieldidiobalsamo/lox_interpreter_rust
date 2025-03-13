use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum OpCode {
    OpReturn,
    OpConstant(u8),
}

impl Display for OpCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::OpReturn => write!(f, "OP_RETURN"),
            OpCode::OpConstant(_) => write!(f, "OP_CONSTANT"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Chunk {
    code: Vec<OpCode>, // Vec is contiguous so CPU cache friendly (dense storage)
    constants: Vec<Value>,
    lines: Vec<usize>,
}

impl Chunk {
    pub fn write(&mut self, byte: OpCode, line: usize) {
        self.code.push(byte);
        self.lines.push(line);
    }

    pub fn disassemble(&self) -> &[OpCode] {
        &self.code
    }

    pub fn disassemble_to_string(&self, name: &str) -> String {
        format!("== {name} ==\n{}", self)
    }

    pub fn add_constant(&mut self, value: Value) -> u8 {
        self.constants.push(value);
        (self.constants.len() - 1) as u8
    }

    pub fn get_instruction(&self, i: usize) -> Option<&OpCode> {
        self.code.get(i)
    }
}

impl Display for Chunk {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut out = String::new();

        for (offset, opcode) in self.code.iter().enumerate() {
            out += &format!("{offset:04}");

            if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
                out += &format!(" | {opcode}");
            } else {
                out += &format!(" {opcode} {}", self.lines[offset]);
            }

            out += &(match opcode {
                OpCode::OpConstant(index) => format!(" '{}'", self.constants[*index as usize]),
                _ => String::new(), // nothing to add
            } + "\n");
        }

        write!(f, "{out}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sample() {
        let mut chunk = Chunk::default();
        chunk.write(OpCode::OpReturn, 0);
        assert_eq!(chunk.disassemble(), &[OpCode::OpReturn]);
    }

    #[test]
    fn disassemble_to_string() {
        let mut chunk = Chunk::default();
        chunk.write(OpCode::OpReturn, 0);

        let good = String::from("== test chunk ==\n0000 OP_RETURN 0\n");

        assert_eq!(chunk.disassemble_to_string("test chunk"), good);
    }

    #[test]
    fn constant() {
        let mut chunk = Chunk::default();
        let constant = chunk.add_constant(Value::Number(1.2));
        chunk.write(OpCode::OpConstant(constant), 0);

        let constant = chunk.add_constant(Value::Number(5.));
        chunk.write(OpCode::OpConstant(constant), 1);
        chunk.write(OpCode::OpReturn, 2);

        assert_eq!(
            chunk.disassemble(),
            &[
                OpCode::OpConstant(0),
                OpCode::OpConstant(1),
                OpCode::OpReturn
            ]
        );

        let good = "== test chunk ==
0000 OP_CONSTANT 0 '1.2'
0001 OP_CONSTANT 1 '5'
0002 OP_RETURN 2
";
        assert_eq!(chunk.disassemble_to_string("test chunk"), good);
    }

    #[test]
    fn same_line() {
        let mut chunk = Chunk::default();
        let constant = chunk.add_constant(Value::Number(1.2));
        chunk.write(OpCode::OpConstant(constant), 0);

        let constant = chunk.add_constant(Value::Number(5.));
        chunk.write(OpCode::OpConstant(constant), 0);
        chunk.write(OpCode::OpReturn, 1);

        let good = "== test chunk ==
0000 OP_CONSTANT 0 '1.2'
0001 | OP_CONSTANT '5'
0002 OP_RETURN 1
";
        assert_eq!(chunk.disassemble_to_string("test chunk"), good);
    }
}

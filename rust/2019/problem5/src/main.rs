use std::io::{stdin, Read};
use crate::Parameter::{Position, Immediate};

fn main() {
    let input = get_input();
    let solution = solve(&input);

    println!("The solution to part 1 is {}", solution);
}

fn solve(s: &str) -> i32 {
    let parsed_input = parse(s);
    let mut m = Machine::new(&parsed_input, 1);
    m.run_until_halt()
}

fn parse(s: &str) -> Vec<i32> {
    s.split(',')
        .into_iter()
        .map(|s| s.parse().unwrap())
        .collect()
}

fn get_input() -> String {
    let mut string = String::new();
    stdin().lock()
        .read_to_string(&mut string)
        .unwrap();
    string
}

struct Machine {
    ip: usize,
    memory: [i32; 10000],
    input: i32,
    output: i32,
}

#[derive(Debug, Eq, PartialEq)]
enum Opcode {
    Add(Parameter, Parameter, Parameter),
    Multiply(Parameter, Parameter, Parameter),
    Input(Parameter),
    Output(Parameter),
    Halt,
}

fn decode_opcode(code: &[i32]) -> (Opcode, usize) {
    let a = &code[0];
    let t = a % 100;
    let params = &code[1..];
    match t {
        1 => decode_add(a, params),
        2 => decode_multiply(a, params),
        3 => decode_input(a, params),
        4 => decode_output(a, params),
        99 => (Opcode::Halt, 1),
        op => panic!("Unknown opcode: {}", op)
    }
}

fn decode_add(a: &i32, ps: &[i32]) -> (Opcode, usize) {
    let (p0, p1, p2) = get_arithmetic_params(a, ps);

    (Opcode::Add(p0, p1, p2), 4)
}

fn decode_multiply(a: &i32, ps: &[i32])-> (Opcode, usize) {
    let (p0, p1, p2) = get_arithmetic_params(a, ps);

    (Opcode::Multiply(p0, p1, p2), 4)
}

fn decode_input(a: &i32, ps: &[i32])-> (Opcode, usize) {
    (Opcode::Input(get_input_param(a, ps)), 2)
}

fn decode_output(a: &i32, ps: &[i32])-> (Opcode, usize) {
    (Opcode::Output(get_output_param(a, ps)), 2)
}

fn get_arithmetic_params(a: &i32, ps: &[i32]) -> (Parameter, Parameter, Parameter) {
    let p0_type = (a / 100) % 10;
    let p1_type = (a / 1000) % 10;
    let p2_type = (a / 10000) % 10;

    let p0 = decode_param(p0_type, ps[0]);
    let p1 = decode_param(p1_type, ps[1]);
    let p2 = decode_param(p2_type, ps[2]);

    (p0, p1, p2)
}

fn get_input_param(a: &i32, ps: &[i32]) -> Parameter {
    let mode = *a / 100;
    if mode != 0 {
        eprintln!("mode was {}", *a);
        panic!("input param mode must be position")
    }
    decode_param(0, ps[0])
}

fn get_output_param(a: &i32, ps: &[i32]) -> Parameter {
    let mode = *a / 100;
    decode_param(mode, ps[0])
}

fn decode_param(t: i32, v: i32) -> Parameter {
    match t {
        0 => Position(v as usize),
        1 => Immediate(v),
        _ => unimplemented!(),
    }
}

#[derive(Debug, Eq, PartialEq)]
enum Parameter {
    Immediate(i32),
    Position(usize),
}

impl Machine {
    fn new(initial_memory: &[i32], input: i32) -> Machine {
        let mut memory = [0; 10000];
        for i in 0..initial_memory.len() {
            memory[i] = initial_memory[i];
        }
        Machine { ip: 0, memory, input, output: 0 }
    }

    fn run_until_halt(&mut self) -> i32 {
        loop {
            let (op, op_size) = self.fetch(self.ip);
            println!("running {:?}", op);
            let halted = self.execute(&op);
            self.ip += op_size;
            match halted {
                true => break,
                _ => continue,
            }
        }
        self.output
    }

    fn fetch(&self, addr: usize) -> (Opcode, usize) {
        decode_opcode(&self.memory[addr..addr +4])
    }

    fn execute(&mut self, op: &Opcode) -> bool {
        use Opcode::*;

        if self.output != 0 {
            if let Halt = op {
                println!("test passed")
            } else {
                panic!("test failed")
            }
        }
        match op {
            Add(a, b, c) => {
                let a = self.evaluate_param(a);
                let b = self.evaluate_param(b);
                println!("{} + {} = {}", a, b, a + b);
                self.write(c, a + b);
                false
            },
            Multiply(a, b, c) => {
                let a = self.evaluate_param(a);
                let b = self.evaluate_param(b);
                println!("{} * {} = {}", a, b, a * b);
                self.write(c, a * b);
                false
            },
            Input(addr) => {
                self.write(addr, self.input);
                false
            },
            Output(addr) => {
                let value = self.evaluate_param(addr);
                println!("output {}", value);
                self.output = value;
                false
            }
            Halt => true,
        }
    }

    fn write(&mut self, addr: &Parameter, value: i32) {
        use Parameter::*;

        match addr {
            Position(addr) => {
                println!("writing {} to *{}", value, *addr);
                self.memory[*addr] = value
            },
            _ => panic!("Writing to immediate parameter not supported"),
        }
    }

    fn evaluate_param(&self, p: &Parameter) -> i32 {
        use Parameter::*;

        match p {
            Immediate(value) => *value,
            Position(addr) => self.memory[*addr],
        }
    }
}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_decode_opcode() {
        use Opcode::*;
        use Parameter::*;

        assert_eq!(decode_opcode(&[1, 0, 0, 0]), (Add(Position(0), Position(0), Position(0)), 4));
        assert_eq!(decode_opcode(&[1101, 0, 0, 0]), (Add(Immediate(0), Immediate(0), Position(0)), 4));
        assert_eq!(decode_opcode(&[102, 0, 13, 0]), (Multiply(Immediate(0), Position(13), Position(0)), 4));
        assert_eq!(decode_opcode(&[03, 1231]), (Input(Position(1231)), 2));
        assert_eq!(decode_opcode(&[04, 1111]), (Output(Position(1111)), 2));
        assert_eq!(decode_opcode(&[104, 1111]), (Output(Immediate(1111)), 2));
    }

    #[test]
    fn test_io() {
        let program = [3,0,4,0,99];
        let mut m = Machine::new(&program, 42);
        let result = m.run_until_halt();
        assert_eq!(result, 42)
    }
}

use std::io::{stdin, Read};
use crate::Parameter::{Position, Immediate};

fn main() {
    let input = get_input();
    let solution = solve(&input);

    println!("The solution to part 1 is {}", solution);

    let part2 = solve2(&input);

    println!("The solution to part 2 is {}", part2);
}

fn solve(s: &str) -> i32 {
    let parsed_input = parse(s);
    let mut m = Machine::new(&parsed_input, 1);
    m.run_until_halt()
}

fn solve2(s: &str) -> i32 {
    let parsed_input = parse(s);
    let mut m = Machine::new(&parsed_input, 5);
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
    JumpIfTrue(Parameter, Parameter),
    JumpIfFalse(Parameter, Parameter),
    LessThan(Parameter, Parameter, Parameter),
    EqualTo(Parameter, Parameter, Parameter),
}

enum IpUpdate {
    Relative(i32),
    Absolute(usize),
}

fn decode_opcode(code: &[i32]) -> Opcode {
    let a = &code[0];
    let t = a % 100;
    let params = &code[1..];
    match t {
        1 => decode_add(a, params),
        2 => decode_multiply(a, params),
        3 => decode_input(a, params),
        4 => decode_output(a, params),
        5 => decode_jump_if_true(a, params),
        6 => decode_jump_if_false(a, params),
        7 => decode_less_than(a, params),
        8 => decode_equal_to(a, params),
        99 => Opcode::Halt,
        op => panic!("Unknown opcode: {}", op)
    }
}

fn decode_add(a: &i32, ps: &[i32]) -> Opcode {
    let (p0, p1, p2) = get_arithmetic_params(a, ps);

    Opcode::Add(p0, p1, p2)
}

fn decode_multiply(a: &i32, ps: &[i32])-> Opcode {
    let (p0, p1, p2) = get_arithmetic_params(a, ps);

    Opcode::Multiply(p0, p1, p2)
}

fn decode_input(a: &i32, ps: &[i32])-> Opcode {
    Opcode::Input(get_input_param(a, ps))
}

fn decode_output(a: &i32, ps: &[i32])-> Opcode {
    Opcode::Output(get_output_param(a, ps))
}

fn decode_jump_if_true(a: &i32, ps: &[i32]) -> Opcode {
    let (p0, p1) = get_jump_params(a, ps);

    Opcode::JumpIfTrue(p0, p1)
}

fn decode_jump_if_false(a: &i32, ps: &[i32]) -> Opcode {
    // if the first parameter is non-zero, it sets the instruction pointer to
    // the value from the second parameter. Otherwise, it does nothing.
    let (p0, p1) = get_jump_params(a, ps);

    Opcode::JumpIfFalse(p0, p1)
}

fn decode_less_than(a: &i32, ps: &[i32]) -> Opcode {
    // if the first parameter is non-zero, it sets the instruction pointer to
    // the value from the second parameter. Otherwise, it does nothing.
    let (p0, p1, p2) = get_arithmetic_params(a, ps);

    Opcode::LessThan(p0, p1, p2)
}

fn decode_equal_to(a: &i32, ps: &[i32]) -> Opcode {
    // if the first parameter is non-zero, it sets the instruction pointer to
    // the value from the second parameter. Otherwise, it does nothing.
    let (p0, p1, p2) = get_arithmetic_params(a, ps);

    Opcode::EqualTo(p0, p1, p2)
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

fn get_jump_params(a: &i32, ps: &[i32]) -> (Parameter, Parameter) {
    let p0_type = (a / 100) % 10;
    let p1_type = (a / 1000) % 10;

    let p0 = decode_param(p0_type, ps[0]);
    let p1 = decode_param(p1_type, ps[1]);

    (p0, p1)
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
            let op = self.fetch(self.ip);
            println!("running {:?}", op);
            let (halted, ip_update) = self.execute(&op);
            self.update_ip(&ip_update);
            match halted {
                true => break,
                _ => continue,
            }
        }
        self.output
    }

    fn update_ip(&mut self, update: &IpUpdate) {
        match update {
            IpUpdate::Relative(n) => self.ip += *n as usize,
            IpUpdate::Absolute(n) => self.ip = *n,
        }
    }

    fn fetch(&self, addr: usize) -> Opcode {
        decode_opcode(&self.memory[addr..addr +4])
    }

    fn execute(&mut self, op: &Opcode) -> (bool, IpUpdate) {
        use Opcode::*;
        use IpUpdate::*;

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
                (false, Relative(4))
            },
            Multiply(a, b, c) => {
                let a = self.evaluate_param(a);
                let b = self.evaluate_param(b);
                println!("{} * {} = {}", a, b, a * b);
                self.write(c, a * b);
                (false, Relative(4))
            },
            Input(addr) => {
                self.write(addr, self.input);
                (false, Relative(2))
            },
            Output(addr) => {
                let value = self.evaluate_param(addr);
                println!("output {}", value);
                self.output = value;
                (false, Relative(2))
            }
            JumpIfTrue(a, b) => {
                let a = self.evaluate_param(a);
                let b = self.evaluate_param(b);
                let branch = if a != 0 {
                    Absolute(b as usize)
                } else {
                    Relative(3)
                };
                (false, branch)
            },
            JumpIfFalse(a, b) => {
                let a = self.evaluate_param(a);
                let b = self.evaluate_param(b);
                let branch = if a == 0 {
                    Absolute(b as usize)
                } else {
                    Relative(3)
                };
                (false, branch)
            }
            Halt => (true, Relative(1)),
            LessThan(a, b, c) => {
                let a = self.evaluate_param(a);
                let b = self.evaluate_param(b);
                let result = if a < b {
                    1
                } else {
                    0
                };
                self.write(c, result);
                (false, Relative(4))
            },
            EqualTo(a, b, c) => {
                let a = self.evaluate_param(a);
                let b = self.evaluate_param(b);
                let result = if a == b {
                    1
                } else {
                    0
                };
                self.write(c, result);
                (false, Relative(4))
            }
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

        assert_eq!(decode_opcode(&[1, 0, 0, 0]), Add(Position(0), Position(0), Position(0)));
        assert_eq!(decode_opcode(&[1101, 0, 0, 0]), Add(Immediate(0), Immediate(0), Position(0)));
        assert_eq!(decode_opcode(&[102, 0, 13, 0]), Multiply(Immediate(0), Position(13), Position(0)));
        assert_eq!(decode_opcode(&[03, 1231]), Input(Position(1231)));
        assert_eq!(decode_opcode(&[04, 1111]), Output(Position(1111)));
        assert_eq!(decode_opcode(&[104, 1111]), Output(Immediate(1111)));
    }

    #[test]
    fn test_io() {
        let program = [3,0,4,0,99];
        let mut m = Machine::new(&program, 42);
        let result = m.run_until_halt();
        assert_eq!(result, 42)
    }
}

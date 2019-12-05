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
}

fn decode_opcode(code: &[i32]) -> (Opcode, usize) {
    let a = code[0];
    let t = a % 100;
    match t {
        1 => decode_add(&a, &code[1..]),
        _ => unimplemented!()
    }
}

fn decode_add(a: &i32, ps: &[i32]) -> (Opcode, usize) {
    use Opcode::*;

    let p0_type = (a / 100) % 10;
    let p1_type = (a / 1000) % 10;
    let p2_type = (a / 10000) % 10;

    let p0 = decode_param(p0_type, ps[0]);
    let p1 = decode_param(p1_type, ps[1]);
    let p2 = decode_param(p2_type, ps[2]);

    (Add(p0, p1, p2), 4)
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
            let op = self.fetch();
            let (halted, ip_incr) = self.execute(&op);
            match halted {
                true => break,
                _ => continue,
            }
        }
        self.output
    }

    fn get_value_at_addr(&self, addr: usize) -> i32 {
        self.memory[addr]
    }

    fn fetch(&self) -> Opcode {
        unimplemented!()
    }

    fn execute(&mut self, op: &Opcode) -> (bool, usize) {
        unimplemented!()
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
        assert_eq!(decode_opcode(&[1002, 0, 13, 0]), (Multiply(Immediate(0), Position(13), Position(0)), 4));
    }
}

pub struct Machine {
    ip: usize,
    memory: [i64; 10000],
    input: Vec<i64>,
    output: Vec<i64>,
    test_mode: bool,
}

impl Machine {
    pub fn new_test_mode(initial_memory: &[i64], input: &[i64]) -> Machine {
        let mut memory = [0; 10000];
        for i in 0..initial_memory.len() {
            memory[i] = initial_memory[i];
        }
        Machine { ip: 0, memory, input: Vec::from(input), output: vec![], test_mode: true }
    }

    pub fn new_with_noun_verb(initial_memory: &[i64], noun: i64, verb: i64) -> Machine {
        let mut m = Machine::new_test_mode(initial_memory, &vec![0]);
        m.memory[1] = noun;
        m.memory[2] = verb;
        m
    }

    pub fn new_feedback_mode(initial_memory: &[i64]) -> Machine {
        let mut m = Machine::new_test_mode(initial_memory, &vec![]);
        m.test_mode = false;
        m
    }

    pub fn input(&mut self, input: i64) {
        self.input.push(input);
    }

    pub fn output(&mut self) -> i64 {
        self.output.remove(0)
    }

    pub fn run(&mut self) -> StoppedState {
        use State::*;

        loop {
            let op = self.fetch(self.ip);
//            println!("running {:?}", op);
            let state = self.execute(&op);
            match state {
                Running(ip_delta) => self.update_ip(&ip_delta),
                Stopped(stopped_state) => return stopped_state,
            }
        }
    }

    pub fn get_value_at_addr(&self, addr: usize) -> i64 {
        self.memory[addr]
    }

    fn update_ip(&mut self, update: &IpUpdate) {
        match update {
            IpUpdate::Relative(n) => self.ip += *n as usize,
            IpUpdate::Absolute(n) => self.ip = *n,
        }
    }

    fn fetch(&self, addr: usize) -> Opcode {
        decode_opcode(&self.memory[addr..addr + 4])
    }

    fn execute(&mut self, op: &Opcode) -> State {
        use Opcode::*;
        use IpUpdate::*;
        use State::*;
        use StoppedState::*;

        let test_failed: bool = if self.test_mode {
            let output = self.output.get(0);
//            println!("output {:?}", output);
            match output {
                Some(0) => {
                    self.output.remove(0);
                    false
                }
                None => false,
                _ => true,
            }
        } else {
            false
        };

        let state = match op {
            Add(a, b, c) => {
                let a = self.evaluate_param(a);
                let b = self.evaluate_param(b);
//                println!("{} + {} = {}", a, b, a + b);
                self.write(c, a + b);
                Running(Relative(4))
            }
            Multiply(a, b, c) => {
                let a = self.evaluate_param(a);
                let b = self.evaluate_param(b);
//                println!("{} * {} = {}", a, b, a * b);
                self.write(c, a * b);
                Running(Relative(4))
            }
            Input(addr) => {
                if self.input.is_empty() {
                    return Stopped(BlockedOnInput);
                }
                let a = self.input.remove(0);
                self.write(addr, a);
                Running(Relative(2))
            }
            Output(addr) => {
                let value = self.evaluate_param(addr);
//                println!("output {}", value);
                self.output.push(value);
                Running(Relative(2))
            }
            JumpIfTrue(a, b) => {
                let a = self.evaluate_param(a);
                let b = self.evaluate_param(b);
                let branch = if a != 0 {
                    Absolute(b as usize)
                } else {
                    Relative(3)
                };
                Running(branch)
            }
            JumpIfFalse(a, b) => {
                let a = self.evaluate_param(a);
                let b = self.evaluate_param(b);
                let branch = if a == 0 {
                    Absolute(b as usize)
                } else {
                    Relative(3)
                };
                Running(branch)
            }
            Halt => Stopped(Halted),
            LessThan(a, b, c) => {
                let a = self.evaluate_param(a);
                let b = self.evaluate_param(b);
                let result = if a < b {
                    1
                } else {
                    0
                };
                self.write(c, result);
                Running(Relative(4))
            }
            EqualTo(a, b, c) => {
                let a = self.evaluate_param(a);
                let b = self.evaluate_param(b);
                let result = if a == b {
                    1
                } else {
                    0
                };
                self.write(c, result);
                Running(Relative(4))
            }
        };

        if *op == Halt {
            state
        } else if test_failed {
            panic!("test failed state: {:?} op: {:?}", state, op)
        } else {
            state
        }
    }

    fn write(&mut self, addr: &Parameter, value: i64) {
        use Parameter::*;

        match addr {
            Position(addr) => {
//                println!("writing {} to *{}", value, *addr);
                self.memory[*addr] = value
            }
            _ => panic!("Writing to immediate parameter not supported"),
        }
    }

    fn evaluate_param(&self, p: &Parameter) -> i64 {
        use Parameter::*;

        match p {
            Immediate(value) => *value,
            Position(addr) => self.memory[*addr],
        }
    }
}

pub fn parse_program(s: &str) -> Vec<i64> {
    s.split(',')
        .into_iter()
        .map(|s| s.parse().unwrap())
        .collect()
}

#[derive(Debug)]
enum State {
    Running(IpUpdate),
    Stopped(StoppedState),
}

#[derive(Debug, Eq, PartialEq)]
pub enum StoppedState {
    Halted,
    BlockedOnInput,
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

#[derive(Debug)]
enum IpUpdate {
    Relative(i64),
    Absolute(usize),
}

fn decode_opcode(code: &[i64]) -> Opcode {
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

fn decode_add(a: &i64, ps: &[i64]) -> Opcode {
    let (p0, p1, p2) = get_arithmetic_params(a, ps);

    Opcode::Add(p0, p1, p2)
}

fn decode_multiply(a: &i64, ps: &[i64]) -> Opcode {
    let (p0, p1, p2) = get_arithmetic_params(a, ps);

    Opcode::Multiply(p0, p1, p2)
}

fn decode_input(a: &i64, ps: &[i64]) -> Opcode {
    Opcode::Input(get_input_param(a, ps))
}

fn decode_output(a: &i64, ps: &[i64]) -> Opcode {
    Opcode::Output(get_output_param(a, ps))
}

fn decode_jump_if_true(a: &i64, ps: &[i64]) -> Opcode {
    let (p0, p1) = get_jump_params(a, ps);

    Opcode::JumpIfTrue(p0, p1)
}

fn decode_jump_if_false(a: &i64, ps: &[i64]) -> Opcode {
    // if the first parameter is non-zero, it sets the instruction pointer to
    // the value from the second parameter. Otherwise, it does nothing.
    let (p0, p1) = get_jump_params(a, ps);

    Opcode::JumpIfFalse(p0, p1)
}

fn decode_less_than(a: &i64, ps: &[i64]) -> Opcode {
    // if the first parameter is non-zero, it sets the instruction pointer to
    // the value from the second parameter. Otherwise, it does nothing.
    let (p0, p1, p2) = get_arithmetic_params(a, ps);

    Opcode::LessThan(p0, p1, p2)
}

fn decode_equal_to(a: &i64, ps: &[i64]) -> Opcode {
    // if the first parameter is non-zero, it sets the instruction pointer to
    // the value from the second parameter. Otherwise, it does nothing.
    let (p0, p1, p2) = get_arithmetic_params(a, ps);

    Opcode::EqualTo(p0, p1, p2)
}

fn get_arithmetic_params(a: &i64, ps: &[i64]) -> (Parameter, Parameter, Parameter) {
    let p0_type = (a / 100) % 10;
    let p1_type = (a / 1000) % 10;
    let p2_type = (a / 10000) % 10;

    let p0 = decode_param(p0_type, ps[0]);
    let p1 = decode_param(p1_type, ps[1]);
    let p2 = decode_param(p2_type, ps[2]);

    (p0, p1, p2)
}

fn get_input_param(a: &i64, ps: &[i64]) -> Parameter {
    let mode = *a / 100;
    if mode != 0 {
        eprintln!("mode was {}", *a);
        panic!("input param mode must be position")
    }
    decode_param(0, ps[0])
}

fn get_output_param(a: &i64, ps: &[i64]) -> Parameter {
    let mode = *a / 100;
    decode_param(mode, ps[0])
}

fn get_jump_params(a: &i64, ps: &[i64]) -> (Parameter, Parameter) {
    let p0_type = (a / 100) % 10;
    let p1_type = (a / 1000) % 10;

    let p0 = decode_param(p0_type, ps[0]);
    let p1 = decode_param(p1_type, ps[1]);

    (p0, p1)
}

fn decode_param(t: i64, v: i64) -> Parameter {
    use Parameter::*;

    match t {
        0 => Position(v as usize),
        1 => Immediate(v),
        _ => unimplemented!(),
    }
}

#[derive(Debug, Eq, PartialEq)]
enum Parameter {
    Immediate(i64),
    Position(usize),
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
        let program = [3, 0, 4, 0, 99];
        let mut m = Machine::new_feedback_mode(&program);
        m.input(42);
        let result = m.run();
        assert_eq!(m.output(), 42);
        assert_eq!(result, StoppedState::Halted)
    }
}

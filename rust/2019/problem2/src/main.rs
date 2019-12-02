use std::io::{stdin, Read};

fn main() {
    let input = get_input();
    let solution = solve(&input, 12, 2);

    println!("The solution to part 1 is {}", solution);

    let part_2 = solve_b(&input);
    println!("The solution to part 2 is {}", part_2);
}

fn solve(s: &str, noun: usize, verb: usize) -> usize {
    let parsed_input = parse(s);
    let mut m = Machine::new(&parsed_input, noun, verb);
    m.run_until_halt();
    m.get_value_at_addr(0)
}

const EXPECTED: usize = 19690720;

fn solve_b(s: &str) -> usize {
    let parsed_input = parse(s);
    let mut answer = 0;
    for noun in 0..100 {
        for verb in 0..100 {
            let mut m = Machine::new(&parsed_input, noun, verb);
            m.run_until_halt();
            let a = m.get_value_at_addr(0);
            if a == EXPECTED {
                answer = (100 * noun) + verb
            }
        }
    }

    answer
}

fn parse(s: &str) -> Vec<usize> {
    s.split(',')
        .into_iter()
        .map(|s| s.parse::<usize>().unwrap())
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
    memory: [usize; 10000]
}

impl Machine {
    fn new(initial_memory: &[usize], noun: usize, verb: usize) -> Machine {
        let mut memory = [0; 10000];
        for i in 0..initial_memory.len() {
            memory[i] = initial_memory[i] as usize;
        }
        memory[1] = noun;
        memory[2] = verb;
        Machine { ip: 0, memory }
    }

    fn run_until_halt(&mut self) {
        loop {
            let ip = self.ip;
            let instruction = self.memory[ip];
            let a = self.memory[ip + 1];
            let b = self.memory[ip + 2];
            let c = self.memory[ip + 3] as usize;
            match instruction {
                1 => self.memory[c] = self.memory[a] + self.memory[b],
                2 => self.memory[c] = self.memory[a] * self.memory[b],
                99 => break,
                _ => unimplemented!(),
            }
            self.ip += 4
        }
    }

    fn get_value_at_addr(&self, addr: usize) -> usize {
        self.memory[addr]
    }
}


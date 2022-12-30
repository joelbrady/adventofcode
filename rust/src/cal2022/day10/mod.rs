use nom::{IResult, Parser};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{line_ending, space1};
use nom::multi::separated_list1;
use nom::sequence::separated_pair;

pub fn main() {
    let input = include_str!("input");

    let input = parse_input(input);

    let part1 = solve_part1(&input);

    println!("The solution to part 1 is {}", part1);

    solve_part2(&input);
}

#[derive(Debug, Eq, PartialEq)]
struct Input {
    instructions: Vec<Instruction>,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
enum Instruction {
    NoOp,
    AddX(i64),
}

impl Instruction {
    fn cycles(&self) -> i64 {
        match self {
            Instruction::NoOp => 1,
            Instruction::AddX(_) => 2,
        }
    }
}

fn parse_input(s: &str) -> Input {
    let (_, instructions) = separated_list1(line_ending, parse_instruction)(s).unwrap();
    Input { instructions }
}

fn parse_instruction(s: &str) -> IResult<&str, Instruction> {
    let add_x = separated_pair(tag("addx"), space1, nom::character::complete::i64)
        .map(|(_, n)| Instruction::AddX(n));
    let no_op = tag("noop")
        .map(|_| Instruction::NoOp);

    alt((
        no_op,
        add_x,
    ))(s)
}

struct Machine {
    x: i64,
    cycles: i64,
    program: Vec<Instruction>,
    current_instruction: Executing,
    running: bool,
}

#[derive(Debug)]
enum Executing {
    Nothing,
    // the instruction to do, and how many cycles until it takes effect
    Instruction(Instruction, i64),
}

impl Machine {
    fn new(program: &[Instruction]) -> Self {
        Self {
            x: 1,
            cycles: 0,
            program: program.to_vec(),
            current_instruction: Executing::Nothing,
            running: false,
        }
    }

    fn halted(&self) -> bool {
        !self.running
    }

    fn start(&mut self) {
        self.running = true;
    }

    fn next_cycle(&mut self) -> (i64, i64) {
        match &mut self.current_instruction {
            Executing::Nothing => {
                self.fetch_next_instruction();
                if self.running {
                    self.next_cycle()
                } else {
                    (self.x, self.cycles)
                }
            }
            Executing::Instruction(i, n) => {
                let x = if *n == 0 {
                    let x = match i {
                        Instruction::NoOp => {
                            self.x
                        }
                        Instruction::AddX(k) => {
                            let old = self.x;
                            self.x += *k;
                            old
                        },
                    };
                    self.current_instruction = Executing::Nothing;
                    self.fetch_next_instruction();
                    x
                } else {
                    *n -= 1;
                    self.x
                };
                self.cycles += 1;
                (x, self.cycles)
            }
        }
    }

    fn fetch_next_instruction(&mut self) {
        if self.program.is_empty() {
            self.running = false;
        } else {
            let i = self.program.remove(0);
            self.current_instruction = Executing::Instruction(i, i.cycles() - 1);
        }
    }
}

fn solve_part1(input: &Input) -> i64 {
    let mut m = Machine::new(&input.instructions);
    let mut signal_strengths = vec![];

    m.start();

    while !m.halted() {
        let (intermediate_x, cycle_in_progress) = m.next_cycle();
        if cycle_in_progress >= 20 && (cycle_in_progress - 20) % 40 == 0 {
            signal_strengths.push((cycle_in_progress, intermediate_x));
        }

    }

    signal_strengths.iter()
        .map(|(x, c)| *x * *c)
        .sum()
}

fn solve_part2(input: &Input) {
    let mut m = Machine::new(&input.instructions);

    let mut crt_x = 0;
    m.start();

    while !m.halted() {
        let (sprite_position, _cycle_in_progress) = m.next_cycle();
        let start = sprite_position - 1;
        let end = sprite_position + 1;
        if (start..=end).contains(&crt_x) {
            print!("#");
        } else {
            print!(".")
        }
        if (crt_x + 1) % 40 == 0 {
            println!();
            crt_x = 0;
        } else {
            crt_x += 1;

        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_cycles_for_add() {
        let program = vec![Instruction::AddX(3), Instruction::AddX(-5)];
        let mut m = Machine::new(&program);
        m.start();

        assert_eq!(m.x, 1);
        assert_eq!(m.cycles, 0);

        assert_eq!(m.next_cycle(), (1, 1));
        assert_eq!(m.x, 1);
        assert_eq!(m.cycles, 1);

        assert_eq!(m.next_cycle(), (1, 2));

        assert_eq!(m.x, 4);
        assert_eq!(m.cycles, 2);

        assert_eq!(m.next_cycle(), (4, 3));

        assert_eq!(m.x, 4);
        assert_eq!(m.cycles, 3);

        assert_eq!(m.next_cycle(), (4, 4));

        assert_eq!(m.x, -1);
        assert_eq!(m.cycles, 4);
    }

    #[test]
    fn test_instruction_examples() {
        let program = vec![Instruction::NoOp, Instruction::AddX(3), Instruction::AddX(-5)];
        let mut m = Machine::new(&program);
        m.start();

        assert_eq!(m.cycles, 0);
        assert_eq!(m.x, 1);

        m.next_cycle();

        assert_eq!(m.cycles, 1);
        assert_eq!(m.x, 1);

        m.next_cycle();

        assert_eq!(m.cycles, 2);
        assert_eq!(m.x, 1);

        m.next_cycle();

        assert_eq!(m.cycles, 3);
        assert_eq!(m.x, 4);

        m.next_cycle();

        assert_eq!(m.cycles, 4);
        assert_eq!(m.x, 4);

        m.next_cycle();

        assert_eq!(m.cycles, 5);
        assert_eq!(m.x, -1);

        assert!(m.halted());
    }

    #[test]
    fn test_solve_part1_example() {
        let input = parse_input(include_str!("example"));
        let expected = 13140;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1() {
        let input = parse_input(include_str!("input"));
        let expected = 12840;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2_example() {
        let input = parse_input(include_str!("example"));
        solve_part2(&input);
    }

    #[test]
    fn test_solve_part2() {
        let input = parse_input(include_str!("input"));
        solve_part2(&input);
    }
}
use std::collections::HashSet;

use nom::bytes::complete::is_not;
use nom::character::complete::{line_ending, space1};
use nom::IResult;
use nom::multi::separated_list1;
use nom::sequence::separated_pair;

pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let part1 = solve(&input);
    let part2 = solve2(&input);

    println!("The solution to part 1 is {}", part1);
    println!("The solution to part 2 is {}", part2);
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Instruction {
    Acc(i32),
    Nop(i32),
    Jmp(i32),
}

fn parse_input(input: &str) -> Vec<Instruction> {
    let (_, instructions) = separated_list1(line_ending, parse_instruction)(input).unwrap();

    instructions
}

fn parse_instruction(input: &str) -> IResult<&str, Instruction> {
    let (input, (instruction_name, number)) = separated_pair(is_not(" "), space1, is_not("\r\n"))(input)?;

    let number: i32 = number.parse().unwrap();
    let instruction = match instruction_name {
        "acc" => Instruction::Acc(number),
        "nop" => Instruction::Nop(number),
        "jmp" => Instruction::Jmp(number),
        _ => unimplemented!()
    };

    Ok((input, instruction))
}

fn solve(program: &[Instruction]) -> i32 {
    let mut accumulator: i32 = 0;
    let mut ip: i32 = 0;
    let mut seen = HashSet::new();

    while !seen.contains(&ip) {
        let instruction = program[ip as usize];
        seen.insert(ip);
        match instruction {
            Instruction::Acc(n) => {
                accumulator += n;
                ip += 1;
            },
            Instruction::Nop(_) => {
                ip += 1;
            },
            Instruction::Jmp(offset) => {
                ip += offset;
            },
        }
    }

    accumulator
}

fn solve2(program: &[Instruction]) -> i32 {
    let mutations: Vec<Vec<Instruction>> = mutate(program);

    mutations.iter()
        .filter_map(|program| run(program))
        .next()
        .unwrap()
}

fn mutate(program: &[Instruction]) -> Vec<Vec<Instruction>> {
    let mut mutations = vec![];
    for i in 0..(program.len()) {
        let mut copy: Vec<Instruction> = program.iter().copied().collect();
        copy[i] = match copy[i] {
            Instruction::Acc(n) => Instruction::Acc(n),
            Instruction::Nop(n) => Instruction::Jmp(n),
            Instruction::Jmp(n) => Instruction::Nop(n),
        };
        mutations.push(copy);
    }

    mutations
}

fn run(program: &[Instruction]) -> Option<i32> {
    let max_cycles = 10000;
    let mut cycle_counter = 0;
    let mut accumulator: i32 = 0;
    let mut ip: i32 = 0;

    while cycle_counter < max_cycles && ip < (program.len() as i32) {
        let instruction = program[ip as usize];
        match instruction {
            Instruction::Acc(n) => {
                accumulator += n;
                ip += 1;
            },
            Instruction::Nop(_) => {
                ip += 1;
            },
            Instruction::Jmp(offset) => {
                ip += offset;
            },
        }
        cycle_counter += 1;
    }

    if ip == (program.len() as i32) {
        Some(accumulator)
    } else {
        None
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_example() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = 5;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 2034;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_example_part2() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = 8;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution_part2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 672;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }
}
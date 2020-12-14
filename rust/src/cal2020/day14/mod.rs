use std::collections::HashMap;

pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let part1 = solve(&input);

    println!("The solution to part 1 is {}", part1);

    let part2 = solve2(&input);
    println!("The solution to part 2 is {}", part2);
}

#[derive(Debug)]
struct Input {
    program: Vec<Instruction>,
}

#[derive(Debug)]
enum Instruction {
    SetBitmask(Bitmask),
    MemoryWrite(MemoryWrite),
}

#[derive(Debug, Copy, Clone)]
struct Bitmask {
    bits: [Mask; 36],
}

impl Default for Bitmask {
    fn default() -> Self {
        Bitmask {
            bits: [Mask::NoMask; 36],
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum Mask {
    One,
    Zero,
    NoMask,
}

#[derive(Debug)]
struct MemoryWrite {
    address: usize,
    value: u64,
}

fn parse_input(input: &str) -> Input {
    let program = input.lines()
        .map(|line| parse_line(line))
        .collect();

    Input { program }
}

fn parse_line(input: &str) -> Instruction {
    if input.starts_with("mask") {
        let split: Vec<&str> = input.split(" = ").collect();
        let raw_mask = split[1];
        let mask_bits: Vec<Mask> = raw_mask.chars()
            .map(|c| match c {
                'X' => Mask::NoMask,
                '1' => Mask::One,
                '0' => Mask::Zero,
                _ => unimplemented!()
            })
            .collect();

        assert_eq!(mask_bits.len(), 36);
        let mut bits = [Mask::NoMask; 36];
        for (index, item) in mask_bits.iter().enumerate() {
            bits[index] = *item;
        }

        Instruction::SetBitmask(Bitmask { bits })
    } else {
        let input = &input[4..];
        let input: Vec<&str> = input.split(']').collect();
        let address: usize = input[0].parse().unwrap();
        let value_str = &input[1][3..];
        let value: u64 = value_str.parse().unwrap();
        Instruction::MemoryWrite(MemoryWrite { address, value })
    }
}

#[derive(Debug)]
struct Machine {
    memory: HashMap<usize, u64>,
    bitmask: Bitmask,
}

impl Machine {
    fn new() -> Machine {
        Machine {
            memory: HashMap::new(),
            bitmask: Bitmask::default(),
        }
    }
}

fn solve(input: &Input) -> u64 {
    let mut machine = Machine::new();
    for instruction in input.program.iter() {
        match instruction {
            Instruction::SetBitmask(mask) => {
                machine.bitmask = *mask;
            }
            Instruction::MemoryWrite(write) => {
                let masked_value = apply_mask(&machine.bitmask, write.value);
                machine.memory.insert(write.address, masked_value);
            }
        }
    }

    machine.memory.values()
        .sum()
}

fn apply_mask(bitmask: &Bitmask, value: u64) -> u64 {
    let mut mask: u64 = value;
    for (i, bit) in bitmask.bits.iter().enumerate() {
        let index = 35 - i;
        match bit {
            Mask::One => {
                mask |= 1 << index;
            }
            Mask::Zero => {
                mask &= !(1 << index);
            }
            Mask::NoMask => {}
        }
    }

    mask
}

fn solve2(input: &Input) -> u64 {
    unimplemented!()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_example() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = 165;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 15172047086292;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_example_part2() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = 1068781;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution_part2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 1118684865113056;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }
}

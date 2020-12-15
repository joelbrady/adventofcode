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
            bits: [Mask::X; 36],
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum Mask {
    One,
    Zero,
    X,
}

#[derive(Debug)]
struct MemoryWrite {
    address: u64,
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
        Instruction::SetBitmask(parse_mask(raw_mask))
    } else {
        let input = &input[4..];
        let input: Vec<&str> = input.split(']').collect();
        let address = input[0].parse().unwrap();
        let value_str = &input[1][3..];
        let value: u64 = value_str.parse().unwrap();
        Instruction::MemoryWrite(MemoryWrite { address, value })
    }
}

fn parse_mask(input: &str) -> Bitmask {
    let mask_bits: Vec<Mask> = input.chars()
        .map(|c| match c {
            'X' => Mask::X,
            '1' => Mask::One,
            '0' => Mask::Zero,
            _ => unimplemented!()
        })
        .collect();

    assert_eq!(mask_bits.len(), 36);
    let mut bits = [Mask::X; 36];
    for (index, item) in mask_bits.iter().enumerate() {
        bits[index] = *item;
    }

    Bitmask { bits }
}

#[derive(Debug)]
struct Machine {
    memory: HashMap<u64, u64>,
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
            Mask::X => {}
        }
    }

    mask
}

fn solve2(input: &Input) -> u64 {
    let mut machine = Machine::new();
    for instruction in input.program.iter() {
        match instruction {
            Instruction::SetBitmask(mask) => {
                machine.bitmask = *mask;
            }
            Instruction::MemoryWrite(write) => {
                let masked_addresses = apply_memory_mask(&machine.bitmask, write.address);
                for addr in masked_addresses {
                    machine.memory.insert(addr, write.value);
                }
            }
        }
    }

    machine.memory.values()
        .sum()
}

#[derive(Debug, Copy, Clone)]
enum Bit {
    One,
    Zero,
    Either,
}

fn apply_memory_mask(bitmask: &Bitmask, address: u64) -> Vec<u64> {
    let mut potentials: Vec<Vec<Bit>> = vec![];

    let mut initial = vec![];
    for (j, b) in bitmask.bits.iter().enumerate() {
        let i = 35 - j;
        let bit = get_bit(i, address);
        match b {
            Mask::One => {
                initial.push(Bit::One);
            }
            Mask::Zero => {
                initial.push(bit);
            }
            Mask::X => {
                initial.push(Bit::Either);
            }
        }
    }

    potentials.push(initial);
    let mut addresses = vec![];

    'potential: while let Some(p) = potentials.pop() {
        for (i, b) in p.iter().enumerate() {
            match b {
                Bit::One => {}
                Bit::Zero => {}
                Bit::Either => {
                    let mut a = p.clone();
                    a[i] = Bit::Zero;
                    potentials.push(a);
                    let mut a = p.clone();
                    a[i] = Bit::One;
                    potentials.push(a);
                    continue 'potential;
                }
            }
        }
        addresses.push(flatten(&p));
    }

    addresses
}

fn flatten(bits: &[Bit]) -> u64 {
    let mut output = 0;

    for b in bits.iter() {
        output <<= 1;
        match b {
            Bit::One => {
                output |= 1;
            }
            Bit::Zero => {}
            Bit::Either => {
                panic!()
            }
        }
    }

    output
}

fn get_bit(index: usize, n: u64) -> Bit {
    assert!(index <= 35);
    if (n >> index) & 1 == 1 {
        Bit::One
    } else {
        Bit::Zero
    }
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
    fn test_apply_memory_mask() {
        let bitmask = parse_mask("000000000000000000000000000000X1001X");
        let addr = 42;
        let expected = vec![26, 27, 58, 59];

        let mut actual = apply_memory_mask(&bitmask, addr);

        actual.sort_unstable();

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_example_part2() {
        let input = include_str!("example_part2");
        let input = parse_input(input);

        let expected = 208;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution_part2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 4197941339968;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }
}

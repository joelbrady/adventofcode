use std::collections::{HashMap, VecDeque};

use itertools::Itertools;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{line_ending, space1};
use nom::IResult;
use nom::multi::separated_list1;

pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let part1 = solve_part1(&input);
    println!("The solution to part 1 is {}", part1);

    let part2 = solve_part2(&input);
    println!("The solution to part 2 is {}", part2);
}

#[derive(Debug)]
struct Input {
    monkeys: Vec<Monkey>,
}

#[derive(Debug, Clone)]
struct Monkey {
    name: MonkeyName,
    items: VecDeque<Item>,
    operation: Operation,
    test: Test,
}

#[derive(Debug, Clone)]
struct Item {
    worry_level: u64,
}

#[derive(Debug, Clone, Copy, Eq, Ord, PartialOrd, PartialEq, Hash)]
struct MonkeyName(usize);

#[derive(Debug, Clone)]
enum Operation {
    Add(u64),
    Mul(u64),
    Square
}

#[derive(Debug, Clone)]
struct Test {
    divisor: u64,
    true_branch: MonkeyName,
    false_branch: MonkeyName,
}

fn parse_input(s: &str) -> Input {
    let (_, monkeys) = separated_list1(line_ending, parse_monkey)(s).unwrap();
    Input {
        monkeys,
    }
}

fn parse_monkey(s: &str) -> IResult<&str, Monkey> {
    let (rem, name) = parse_monkey_name(s)?;
    let (rem, items) = parse_items(rem)?;
    let (rem, operation) = parse_operation(rem)?;
    let (rem, test) = parse_test(rem)?;

    let monkey = Monkey {
        name,
        items,
        operation,
        test,
    };

    Ok((rem, monkey))
}

fn parse_monkey_name(s: &str) -> IResult<&str, MonkeyName> {
    let (rem, _) = tag("Monkey ")(s)?;
    let (rem, name) = nom::character::complete::u64(rem)?;
    let (rem, _) = tag(":")(rem)?;
    let (rem, _) = line_ending(rem)?;

    Ok((rem, MonkeyName(name as usize)))
}

fn parse_items(s: &str) -> IResult<&str, VecDeque<Item>> {
    let (rem, _) = space1(s)?;
    let (rem, _) = tag("Starting items: ")(rem)?;
    let (rem, items) = separated_list1(tag(", "), nom::character::complete::u64)(rem)?;
    let (rem, _) = line_ending(rem)?;

    let items: VecDeque<_> = items.into_iter()
        .map(|worry_level| Item { worry_level })
        .collect();

    Ok((rem, items))
}

fn parse_operation(s: &str) -> IResult<&str, Operation> {
    let (rem, _) = space1(s)?;
    let (rem, _) = tag("Operation: new = old ")(rem)?;
    let (rem, op) = alt((
        parse_op_add,
        parse_op_mul,
        parse_op_square,
    ))(rem)?;
    let (rem, _) = line_ending(rem)?;

    Ok((rem, op))
}

fn parse_op_add(s: &str) -> IResult<&str, Operation> {
    let (rem, _) = tag("+ ")(s)?;
    let (rem, n) = nom::character::complete::u64(rem)?;

    Ok((rem, Operation::Add(n)))
}

fn parse_op_mul(s: &str) -> IResult<&str, Operation> {
    let (rem, _) = tag("* ")(s)?;
    let (rem, n) = nom::character::complete::u64(rem)?;

    Ok((rem, Operation::Mul(n)))
}

fn parse_op_square(s: &str) -> IResult<&str, Operation> {
    let (rem, _) = tag("* old")(s)?;

    Ok((rem, Operation::Square))
}

fn parse_test(s: &str) -> IResult<&str, Test> {
    let (rem, _) = space1(s)?;
    let (rem, _) = tag("Test: divisible by ")(rem)?;
    let (rem, divisor) = nom::character::complete::u64(rem)?;
    let (rem, _) = line_ending(rem)?;
    let (rem, _) = space1(rem)?;
    let (rem, _) = tag("If true: throw to monkey ")(rem)?;
    let (rem, true_monkey) = nom::character::complete::u64(rem)?;
    let (rem, _) = line_ending(rem)?;
    let (rem, _) = space1(rem)?;
    let (rem, _) = tag("If false: throw to monkey ")(rem)?;
    let (rem, false_monkey) = nom::character::complete::u64(rem)?;
    let (rem, _) = line_ending(rem)?;

    let test = Test {
        divisor,
        true_branch: MonkeyName(true_monkey as usize),
        false_branch: MonkeyName(false_monkey as usize),
    };

    Ok((rem, test))
}

fn solve_part1(input: &Input) -> u64 {
    let mut monkeys = input.monkeys.clone();

    // assume that the monkeys are labelled 0..n
    for i in 0..monkeys.len() {
        assert!(monkeys.iter().any(|m| m.name.0 == i));
    }

    let n_rounds = 20;

    let mut inspections: HashMap<MonkeyName, u64> = HashMap::new();

    for _ in 0..n_rounds {
        for i in 0..monkeys.len() {
            let mut monkey = monkeys[i].clone();
            let mut to_distribute: HashMap<MonkeyName, Vec<Item>> = HashMap::new();
            while let Some(item) = monkey.items.pop_front() {
                let k = inspections.entry(monkey.name).or_default();
                *k += 1;
                let worry_level = item.worry_level;
                let worry_level = match monkey.operation {
                    Operation::Add(n) => worry_level + n,
                    Operation::Mul(n) => worry_level * n,
                    Operation::Square => worry_level * worry_level,
                };

                let worry_level = worry_level / 3;

                let target_monkey = if worry_level % monkey.test.divisor == 0 {
                    monkey.test.true_branch
                } else {
                    monkey.test.false_branch
                };

                to_distribute.entry(target_monkey).or_default().push(Item { worry_level });

            }
            monkeys[i] = monkey;
            for (name, items) in to_distribute {
                let m = monkeys.iter_mut().find(|m| m.name == name).unwrap();
                m.items.extend(items);
            }
        }
    }

    inspections.into_values().sorted().rev().take(2).product()
}

fn solve_part2(input: &Input) -> u64 {
    let mut monkeys = input.monkeys.clone();

    // assume that the monkeys are labelled 0..n
    for i in 0..monkeys.len() {
        assert!(monkeys.iter().any(|m| m.name.0 == i));
    }

    let modulo: u64 = monkeys.iter().map(|m| m.test.divisor).product();

    let n_rounds = 10000;

    let mut inspections: HashMap<MonkeyName, u64> = HashMap::new();

    for _ in 0..n_rounds {
        for i in 0..monkeys.len() {
            let mut monkey = monkeys[i].clone();
            let mut to_distribute: HashMap<MonkeyName, Vec<Item>> = HashMap::new();
            while let Some(item) = monkey.items.pop_front() {
                let k = inspections.entry(monkey.name).or_default();
                *k += 1;
                let worry_level = item.worry_level;
                let worry_level = match monkey.operation {
                    Operation::Add(n) => worry_level + n,
                    Operation::Mul(n) => worry_level * n,
                    Operation::Square => worry_level * worry_level,
                };

                let worry_level = worry_level % modulo;

                let target_monkey = if worry_level % monkey.test.divisor == 0 {
                    monkey.test.true_branch
                } else {
                    monkey.test.false_branch
                };

                to_distribute.entry(target_monkey).or_default().push(Item { worry_level });

            }
            monkeys[i] = monkey;
            for (name, items) in to_distribute {
                let m = monkeys.iter_mut().find(|m| m.name == name).unwrap();
                m.items.extend(items);
            }
        }
    }

    inspections.into_values().sorted().rev().take(2).product()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_solve_part1_example() {
        let input = parse_input(include_str!("example"));
        let expected = 10605;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1() {
        let input = parse_input(include_str!("input"));
        let expected = 54752;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2_example() {
        let input = parse_input(include_str!("example"));
        let expected = 2713310158;
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2() {
        let input = parse_input(include_str!("input"));
        let expected = 13606755504;
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }
}
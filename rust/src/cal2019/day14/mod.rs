use std::collections::HashMap;

use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, line_ending, space0};
use nom::IResult;
use nom::multi::separated_nonempty_list;
use nom::sequence::{preceded, separated_pair};

use crate::parse::parse_i32;

pub fn main() {
    let input = include_str!("input");
    println!("The solution to part 1 is {}", solve_part1(input));
    println!("The solution to part 1 is {}", solve_part2(input));
}

fn solve_part2(input: &str) -> u64 {
    let (_, formulas) = parse_formulas(&input).unwrap();

    let map: HashMap<&str, Formula> = build_lookup_table(&formulas);

    const LIMIT: u64 = 1_000_000_000_000;

    let mut lower: u64 = 1;
    let mut upper: u64 = LIMIT;

    while lower <= upper {
        let middle = (lower + upper) / 2;
        let mut left_over = init_left_over(&formulas);
        let ore_needed = gen("FUEL", middle, &mut left_over, &map);
        if ore_needed > LIMIT {
            upper = middle - 1;
        } else if ore_needed < LIMIT {
            lower = middle + 1;
        }
    }

    upper
}

fn solve_part1(input: &str) -> u64 {
    let (_, formulas) = parse_formulas(&input).unwrap();

    let map: HashMap<&str, Formula> = build_lookup_table(&formulas);

    let mut left_over = init_left_over(&formulas);

    gen("FUEL", 1, &mut left_over, &map)
}

fn init_left_over<'a>(formulas: &[Formula<'a >]) -> HashMap<&'a str, u64> {
    let mut left_over = HashMap::new();
    formulas.iter()
        .map(|f| f.output.0)
        .for_each(|name| {
            left_over.insert(name, 0);
        });

    left_over
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct Formula<'a> {
    inputs: Vec<(&'a str, u64)>,
    output: (&'a str, u64),
}

fn parse_formulas(input: &str) -> IResult<&str, Vec<Formula>> {
    separated_nonempty_list(line_ending, parse_formula)(input)
}

fn parse_formula<'a>(input: &'a str) -> IResult<&'a str, Formula> {
    // example "7 A, 1 E => 1 FUEL"

    let (input, (inputs, output)) = separated_pair(
        parse_formula_input,
        tag(" => "),
        parse_formula_pair,
    )(input)?;

    Ok((input, Formula { inputs, output }))
}

fn parse_formula_input<'a>(input: &'a str) -> IResult<&str, Vec<(&'a str, u64)>> {
    let separator = preceded(nom::character::complete::char(','), space0);
    let (input, inputs) = separated_nonempty_list(separator, parse_formula_pair)(input)?;

    Ok((input, inputs))
}

fn parse_formula_pair(input: &str) -> IResult<&str, (&str, u64)> {
    let (input, amount) = parse_i32(input)?;
    let (input, _) = space0(input)?;
    let (input, chemical) = alpha1(input)?;

    let result = (chemical, amount as u64);

    Ok((input, result))
}

fn gen<'a>(chemical: &'a str, amount_needed: u64, left_over: &mut HashMap<&'a str, u64>, lookup: &'a HashMap<&'a str, Formula>) -> u64 {
//    println!("gen {} {} left over {:?}", amount_needed, chemical, left_over);
    if chemical == "ORE" {
        return amount_needed;
    }

    let mut amount_needed = amount_needed;
    let mut ore_required = 0;

    let already_available = left_over.get_mut(chemical).unwrap();
    if *already_available > 0 {
        if *already_available >= amount_needed {
            *already_available -= amount_needed;
            return ore_required;
        } else {
            amount_needed -= *already_available;
            *already_available = 0;
        }
    }

    let formula = lookup.get(chemical).unwrap();
    let produced = formula.output.1;
    let times_to_run_formula = if amount_needed % produced == 0 {
        amount_needed / produced
    } else {
        (amount_needed / produced) + 1
    };
    let produced = produced * times_to_run_formula;
    if produced > amount_needed {
        let the_rest = produced - amount_needed;
        *already_available = the_rest;
    }
    for (input_chemical, input_amount) in &formula.inputs {
        let input_needed = times_to_run_formula * *input_amount;
        ore_required += gen(*input_chemical, input_needed, left_over, lookup);
    }

    ore_required
}

fn build_lookup_table<'a>(formulas: &'a [Formula]) -> HashMap<&'a str, Formula<'a>> {
    formulas.iter()
        .map(|f| (f.output.0, f.clone()))
        .collect()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_gen_ore() {
        let ore_needed = gen("ORE", 10, &mut HashMap::new(), &HashMap::new());
        assert_eq!(ore_needed, 10);
    }

    #[test]
    fn test_gen_simple1() {
        let formula = Formula { inputs: vec![("ORE", 13)], output: ("FUEL", 1) };

        let formulas = vec![formula];

        let ore_needed = gen("FUEL", 1, &mut init_left_over(&formulas), &build_lookup_table(&formulas));
        assert_eq!(ore_needed, 13);
    }

    #[test]
    fn test_gen_simple2() {
        let formula = Formula { inputs: vec![("ORE", 1)], output: ("A", 2) };
        let formula2 = Formula { inputs: vec![("A", 3)], output: ("FUEL", 1) };
        let formulas = vec![formula, formula2];
        let mut left_over: HashMap<&str, u64> = init_left_over(&formulas);
        left_over.insert("A", 2);
        let lookup = build_lookup_table(&formulas);
        let ore_needed = gen("FUEL", 1, &mut left_over, &lookup);
        assert_eq!(ore_needed, 1);
        assert_eq!(*left_over.get("A").unwrap(), 1);
    }

    #[test]
    fn test_parse_formula() {
        let example = "7 A, 1 E => 1 FUEL";
        let expected = Formula { inputs: vec![("A", 7), ("E", 1)], output: ("FUEL", 1) };

        let (remainder, formula) = parse_formula(example).unwrap();
        assert_eq!(formula, expected);
        assert_eq!(remainder, "");
    }

    #[test]
    fn test_example1() {
        let answer = solve_part1(include_str!("example1"));
        assert_eq!(answer, 31);
    }

    #[test]
    fn test_example2() {
        let answer = solve_part1(include_str!("example1"));
        assert_eq!(answer, 31);
    }

    #[test]
    fn test_solution_part1() {
        let answer = solve_part1(include_str!("input"));
        assert_eq!(answer, 201_324);
    }

    #[test]
    fn test_part2_example_large1() {
        let answer = solve_part2(include_str!("example_large1"));

        assert_eq!(answer, 82_892_753);
    }

    #[test]
    fn test_part2_solution() {
        let answer = solve_part2(include_str!("input"));

        assert_eq!(answer, 6326857);
    }
}

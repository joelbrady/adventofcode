use input::get_input;
use intcode::{parse_program, Machine};

fn main() {
    let input = get_input("input");
    let solution = solve(&input, 12, 2);

    println!("The solution to part 1 is {}", solution);

    let part_2 = solve_b(&input);
    println!("The solution to part 2 is {}", part_2);
}

fn solve(s: &str, noun: i32, verb: i32) -> i32 {
    let parsed_input = parse_program(s);
    let mut m = Machine::new_with_noun_verb(&parsed_input, noun, verb);
    m.run();
    m.get_value_at_addr(0)
}

const EXPECTED: i32 = 19690720;

fn solve_b(s: &str) -> i32 {
    let parsed_input = parse_program(s);
    let mut answer = 0;
    for noun in 0..100 {
        for verb in 0..100 {
            let mut m = Machine::new_with_noun_verb(&parsed_input, noun, verb);

            m.run();
            let a = m.get_value_at_addr(0);
            if a == EXPECTED {
                answer = (100 * noun) + verb
            }
        }
    }

    answer
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_solutions() {
        let input = get_input("input");
        let solution = solve(&input, 12, 2);

        assert_eq!(solution, 4090701);

        let part_2 = solve_b(&input);

        assert_eq!(part_2, 6421);
    }
}

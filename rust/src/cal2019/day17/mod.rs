use crate::cal2019::intcode::{Machine, parse_program};

pub fn main() {
    let input = include_str!("input");
    let solution = solve1(input);
    println!("The solution to part 1 is {}", solution);

    let main = "B,A,B,B,A,C,A,C,C,A";
    let a = "R,8,L,12,L,12,R,8";
    let b = "L,12,R,8,L,6,R,8,L,6";
    let c = "L,6,R,6,L,12";

    let solution = solve2(input, main, a, b, c);
    println!("The solution to part 2 is {}", solution);
}

fn solve2(input: &str, main: &str, a: &str, b: &str, c: &str) -> i64 {
    let program = parse_program(input);
    let mut m = Machine::new_feedback_mode(&program);
    m.set_memory_at_location(0, 2);
    m.run();
    input_line(main, &mut m);
    input_line(a, &mut m);
    input_line(b, &mut m);
    input_line(c, &mut m);
    input_line("n", &mut m);

    let raw_output = m.dump_output_buffer();
    raw_output[raw_output.len() - 1]
}

fn input_line(line: &str, m: &mut Machine) {
    line.as_bytes()
        .iter()
        .filter(|b| **b != b'\r')
        .for_each(|b| m.input(*b as i64));

    m.input('\n' as i64);
    m.run();
}

fn solve1(input: &str) -> u64 {
    let frame = get_output_part1(input);
    sum_of_alignment_parameters(&frame)
}

fn get_output_part1(input: &str) -> String {
    let program = parse_program(input);
    let mut m = Machine::new_feedback_mode(&program);
    m.run();
    let frame = m.dump_output_buffer();
    convert_frame_to_ascii(&frame)
}

fn convert_frame_to_ascii(frame: &[i64]) -> String {
    let bytes: Vec<u8> = frame.iter()
        .map(|c| *c as u8)
        .collect();

    String::from_utf8(bytes).unwrap()
}

#[derive(Debug)]
struct Intersection(usize, usize);

impl Intersection {
    fn alignment_parameter(&self) -> u64 {
        self.0 as u64 * self.1 as u64
    }
}

fn sum_of_alignment_parameters(input: &str) -> u64 {
    let a = find_intersections(input);
    a.iter()
        .map(|i| i.alignment_parameter())
        .sum()
}

fn find_intersections(frame: &str) -> Vec<Intersection> {
    let mut intersections: Vec<Intersection> = vec![];

    let rows: Vec<&str> = frame.lines().collect();
    let tiles: Vec<Vec<char>> = rows.iter().map(|row| row.chars().collect()).collect();

    for row in 1..(rows.len() - 2) {
        for col in 1..(tiles[0].len() - 1) {
            let tile = tiles[row][col];
            if tile == '#' {
                let north = tiles[row - 1][col];
                let south = tiles[row + 1][col];
                let east = tiles[row][col + 1];
                let west = tiles[row][col - 1];
                if north == '#' && south == '#' && east == '#' && west == '#' {
                    intersections.push(Intersection(row, col))
                }
            }
        }
    }

    intersections
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_example_find_intersections() {
        let input = include_str!("example");

        let example_solution = sum_of_alignment_parameters(&input);

        assert_eq!(example_solution, 76)
    }

    #[test]
    fn test_solve1() {
        let input = include_str!("input");

        let solution = solve1(input);

        assert_eq!(solution, 5068)
    }

    // #[test]
    // fn print_input() {
    //     let input = include_str!("input");
    //     let output = get_output_part1(input);
    //     print!("{}", output)
    // }

    #[test]
    fn part2_debug() {
        let a = "R,8,L,12,L,12,R,8";
        let b = "L,12,R,8,L,6,R,8,L,6";
        let c = "L,6,R,6,L,12";

        let expected = "L,12,R,8,L,6,R,8,L,6,R,8,L,12,L,12,R,8,L,12,R,8,L,6,R,8,L,6,L,12,R,8,L,6,R,8,L,6,R,8,L,12,L,12,R,8,L,6,R,6,L,12,R,8,L,12,L,12,R,8,L,6,R,6,L,12,L,6,R,6,L,12,R,8,L,12,L,12,R,8";

        let actual = vec![b, a, b, b, a, c, a, c, c, a].join(",");

        assert_eq!(actual, expected)
    }

    #[test]
    fn solve_part2() {
        let input = include_str!("input");
        let main = "B,A,B,B,A,C,A,C,C,A";
        let a = "R,8,L,12,L,12,R,8";
        let b = "L,12,R,8,L,6,R,8,L,6";
        let c = "L,6,R,6,L,12";

        let solution = solve2(input, main, a, b, c);

        assert_eq!(solution, 1415975)
    }
}

/*
 L,12,R,8,L,6,R,8,L,6,R,8,L,12,L,12,R,8,L,12,R,8,L,6,R,8,L,6,L,12,R,8,L,6,R,8,L,6,R,8,L,12,L,12,R,8,L,6,R,6,L,12,R,8,L,12,L,12,R,8,L,6,R,6,L,12,L,6,R,6,L,12,R,8,L,12,L,12,R,8
 */

/*
A = L,12,R,8,L,6,R,8,L,6,R,8,L,12,L,12,R,8
B =
C =
 */

/*
Main = B,A,B,B,A,C,A,C,C,A

A = R,8,L,12,L,12,R,8
B = L,12,R,8,L,6,R,8,L,6
C = L,6,R,6,L,12
 */
use crate::cal2019::intcode::{Machine, parse_program};

pub fn main() {
    let input = include_str!("input");
    let solution = solve1(input);
    println!("The solution to part 1 is {}", solution)
}

fn solve1(input: &str) -> i32 {
    let program = parse_program(input);
    let mut m = Machine::new_feedback_mode(&program);
    m.run();
    let frame = m.dump_output_buffer();
    let frame = convert_frame_to_ascii(&frame);
    frame.iter()
        .for_each(|c| print!("{}", c));
    unimplemented!()
}

fn convert_frame_to_ascii(frame: &[i64]) -> Vec<char> {
    frame.iter()
        .map(|c| f(*c))
        .collect()
}

fn f(a: i64) -> char {
    (a as u8) as char
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_solve1() {
        let input = include_str!("input");

        let solution = solve1(input);

        assert_eq!(solution, 0)
    }
}
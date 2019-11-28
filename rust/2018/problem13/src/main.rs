use std::io::Read;

fn main() {
    println!("Hello, world!");
}

struct Track {}

// given a
fn solve(input: &str) -> (i32, i32) {
    unimplemented!()
}

fn get_input_arrays(input: &str) -> Vec<Vec<char>> {
    let input = input.trim_end_matches("\n");
    let lines: Vec<Vec<char>> = input.split("\n")
        .map(|s| s.chars().collect())
        .collect();
    let length = lines[0].len();
    lines.iter()
        .for_each(|line| assert_eq!(line.len(), length));
    lines
}

fn example_input() -> String {
    let mut file = std::fs::File::open("./example.input").unwrap();
    let mut input = String::new();
    let bytes = file.read_to_string(&mut input).unwrap();
    println!("read {} bytes", bytes);
    input
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_solve_example() {
        assert_eq!(solve(example_input().as_str()), (7, 3))
    }

    #[test]
    fn test_get_input_arrays() {
        println!("input: {}", example_input().as_str());
        let arrays = get_input_arrays(example_input().as_str());
        assert_eq!(arrays[0][0], '/');
        assert_eq!(arrays[0][1], '-');
        assert_eq!(arrays[0][2], '>');
        assert_eq!(arrays[1][0], '|');
    }
}

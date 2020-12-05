pub fn main() {
    let input = include_str!("input");

    let part1 = solve(input);
    let part2 = solve2(input);

    println!("The solution to part 1 is {}", part1);
    println!("The solution to part 2 is {}", part2);
}

fn solve(input: &str) -> u32 {
    input.lines()
        .map(|pass| get_id(pass))
        .max()
        .unwrap()
}

fn get_id(pass: &str) -> u32 {
    let (row, col) = parse_boarding_pass(pass);

    (row * 8) + col
}

fn parse_boarding_pass(pass: &str) -> (u32, u32) {
    let mut row_lower = 0;
    let mut row_upper = 127;
    let mut col_lower = 0;
    let mut col_upper = 7;

    let mut last_row = '0';
    let mut last_col = '0';

    pass.chars()
        .for_each(|c| {

            match c {
                'F' => {
                    last_row = 'F';
                    row_upper = ((row_upper - row_lower) / 2) + row_lower
                },
                'B' => {
                    last_row = 'B';
                    row_lower = row_upper - ((row_upper - row_lower) / 2)
                },
                'L' => {
                    last_col = 'L';
                    col_upper = ((col_upper - col_lower) / 2) + col_lower
                },
                'R' => {
                    last_col = 'R';
                    col_lower = col_upper - ((col_upper - col_lower) / 2)
                },
                _ => unimplemented!()
            }
        });

    (row_lower, col_lower)
}

fn solve2(input: &str) -> u32 {
    unimplemented!()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_example1() {
        let input = "FBFBBFFRLR";
        let pass = parse_boarding_pass(input);

        assert_eq!(pass, (44, 5))
    }

    #[test]
    fn test_parse_example2() {
        let input = "BFFFBBFRRR";
        let pass = parse_boarding_pass(input);

        assert_eq!(pass, (70, 7))
    }

    #[test]
    fn test_parse_example3() {
        let input = "FFFBBBFRRR";
        let pass = parse_boarding_pass(input);

        assert_eq!(pass, (14, 7))
    }

    #[test]
    fn test_parse_example4() {
        let input = "BBFFBBFRLL";
        let pass = parse_boarding_pass(input);

        assert_eq!(pass, (102, 4))
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");

        let actual = solve(input);
        let expected = 842;

        assert_eq!(actual, expected)
    }
}
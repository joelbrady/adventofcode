use itertools::Itertools;

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
    heightmap: Vec<Vec<u32>>,
}

fn parse_input(s: &str) -> Input {
    let heightmap = s.lines()
        .map(parse_line)
        .collect();

    Input { heightmap }
}

fn parse_line(s: &str) -> Vec<u32> {
    s.chars()
        .map(|c| if c.is_digit(10) {
            c.to_digit(10).unwrap()
        } else {
            panic!()
        })
        .collect()
}

fn solve(input: &Input) -> u32 {
    let heightmap = &input.heightmap;
    let rows = heightmap.len();
    let cols = heightmap[0].len();

    (0..rows).cartesian_product(0..cols)
        .filter(|(row, col)| {
            let this_h = heightmap[*row][*col];
            let s = surrounding((*row as i64, *col as i64), rows, cols);
            s.into_iter()
                .map(|(r, c)| heightmap[r][c])
                .all(|h| h > this_h)
        })
        .map(|(row, col)| {
            let this_h = heightmap[row][col];
            1 + this_h
        })
        .sum()
}

fn surrounding((row, col): (i64, i64), rows: usize, cols: usize) -> Vec<(usize, usize)> {
    [
        (row - 1, col),
        (row, col - 1),
        (row + 1, col),
        (row, col + 1)
    ]
        .into_iter()
        .filter(|(r, c)| *r >= 0 && *r < rows as i64 && *c >= 0 && *c < cols as i64)
        .map(|(r, c)| (r as usize, c as usize))
        .collect()
}

fn solve2(_: &Input) -> i64 {
    todo!()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_part1_example1() {
        let input = include_str!("example1");
        let input = parse_input(input);
        let expected = 15;

        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 468;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }
}
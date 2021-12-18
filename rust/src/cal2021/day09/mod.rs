use std::collections::HashSet;

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
    heightmap: Heightmap,
}

type Heightmap = Vec<Vec<u32>>;

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
    get_low_points(heightmap).into_iter()
        .map(|(row, col)| {
            let this_h = heightmap[row][col];
            1 + this_h
        })
        .sum()
}

fn get_low_points(heightmap: &Heightmap) -> Vec<(usize, usize)> {
    let rows = heightmap.len();
    let cols = heightmap[0].len();

    (0..rows).cartesian_product(0..cols)
        .filter(|(row, col)| {
            let this_h = heightmap[*row][*col];
            let s = surrounding((*row, *col), rows, cols);
            s.into_iter()
                .map(|(r, c)| heightmap[r][c])
                .all(|h| h > this_h)
        })
        .collect()
}

fn surrounding((row, col): (usize, usize), rows: usize, cols: usize) -> Vec<(usize, usize)> {
    let row = row as i64;
    let col = col as i64;
    let rows = rows as i64;
    let cols = cols as i64;

    [
        (row - 1, col),
        (row, col - 1),
        (row + 1, col),
        (row, col + 1)
    ]
        .into_iter()
        .filter(|(r, c)| *r >= 0 && *r < rows && *c >= 0 && *c < cols)
        .map(|(r, c)| (r as usize, c as usize))
        .collect()
}

fn solve2(input: &Input) -> usize {
    let heightmap = &input.heightmap;
    let low_points = get_low_points(heightmap);
    low_points.iter()
        .map(|p| enumerate_points_in_basin(heightmap, *p))
        .map(|a| a.len())
        .sorted()
        .rev()
        .take(3)
        .product()
}

fn enumerate_points_in_basin(
    heightmap: &Heightmap,
    (row, col): (usize, usize),
) -> Vec<(usize, usize)> {
    let rows = heightmap.len();
    let cols = heightmap[0].len();

    let mut points_in_basin = HashSet::new();

    let mut stack = vec![(row, col)];

    while let Some((row, col)) = stack.pop() {
        let h = heightmap[row][col];
        if h < 9 {
            points_in_basin.insert((row, col));
            surrounding((row, col), rows, cols).into_iter()
                .filter(|p| !points_in_basin.contains(p))
                .for_each(|p| stack.push(p));
        }
    }

    points_in_basin.into_iter()
        .map(|(r, c)| (r, c))
        .collect()
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

    #[test]
    fn test_part2_example1() {
        let input = include_str!("example1");
        let input = parse_input(input);
        let expected = 1134;

        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 1280496;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }
}
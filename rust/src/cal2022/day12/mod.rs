use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashSet};

use nom::character::complete::{line_ending, none_of};
use nom::IResult;
use nom::multi::{many1, separated_list1};

pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let part1 = solve_part1(&input);
    println!("The solution to part 1 is {}", part1);

    let part2 = solve_part2(&input);
    println!("The solution to part 2 is {}", part2);
}

#[derive(Debug, Clone)]
struct Input {
    hill: Vec<Vec<Cell>>,
}

#[derive(Debug, Clone, Copy)]
struct Cell {
    height: i32,
    flag: Flag,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum Flag {
    Normal,
    Start,
    End,
}

fn parse_input(s: &str) -> Input {
    let (_, hill) = separated_list1(line_ending, parse_row)(s).unwrap();

    Input { hill }
}

fn parse_row(s: &str) -> IResult<&str, Vec<Cell>> {
    let (rem, row) = many1(none_of("\r\n"))(s)?;

    let row = row.into_iter()
        .map(|c| {
            if c == 'S' {
                Cell {
                    height: 0,
                    flag: Flag::Start,
                }
            } else if c == 'E' {
                Cell {
                    height: 'z' as i32 - ('a' as i32),
                    flag: Flag::End,
                }
            } else {
                Cell {
                    height: c as i32 - ('a' as i32),
                    flag: Flag::Normal,
                }
            }
        })
        .collect();

    Ok((rem, row))
}

fn solve_part1(input: &Input) -> i32 {
    let grid = &input.hill;
    let start = find_coords_of_flag(grid, Flag::Start);
    let end = find_coords_of_flag(grid, Flag::End);

    dijkstra(grid, start, end).unwrap()
}

fn dijkstra(grid: &[Vec<Cell>], start: (i32, i32), end: (i32, i32)) -> Option<i32> {
    let mut queue: BinaryHeap<Node> = BinaryHeap::new();
    let n_rows = grid.len() as i32;
    let n_cols = grid[0].len() as i32;

    queue.push(Node { distance: 0, coordinates: start, height: 0 });
    let mut visited = HashSet::new();

    while let Some(current) = queue.pop() {
        if visited.contains(&current.coordinates) {
            continue;
        }
        if current.coordinates == end {
            return Some(current.distance);
        }

        let ds = [
            (-1, 0),
            (0, 1),
            (0, -1),
            (1, 0),
        ];

        ds.iter()
            .filter_map(|(dx, dy)| {
                let (x, y) = current.coordinates;
                let (x, y) = (x + dx, y + dy);
                if x >= 0 && y >= 0 && x < n_cols && y < n_rows {
                    let a = &grid[y as usize];
                    let target = &a[x as usize];
                    if target.height <= current.height + 1 {
                        let n = Node { distance: current.distance + 1, coordinates: (x, y), height: target.height };
                        Some(n)
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .for_each(|n| queue.push(n));

        visited.insert(current.coordinates);
    }

    None
}

fn find_coords_of_flag(grid: &[Vec<Cell>], target: Flag) -> (i32, i32) {
    grid.iter()
        .enumerate()
        .flat_map(|(y, row)| row.iter().enumerate().map(move |(x, cell)| (x, y, cell)))
        .find(|(_, _, cell)| matches!(cell.flag, _ if cell.flag == target))
        .map(|(x, y, _)| (x as i32, y as i32))
        .unwrap()
}

fn find_all_a(grid: &[Vec<Cell>]) -> Vec<(i32, i32)> {
    grid.iter()
        .enumerate()
        .flat_map(|(y, row)| row.iter().enumerate().map(move |(x, cell)| (x, y, cell)))
        .filter(|(_, _, cell)| {
            cell.height == 0
        })
        .map(|(x, y, _)| (x as i32, y as i32))
        .collect()
}

#[derive(Debug)]
struct Node {
    distance: i32,
    coordinates: (i32, i32),
    height: i32,
}

impl Ord for Node {
    fn cmp(&self, other: &Self) -> Ordering {
        other.distance.cmp(&self.distance)
    }
}

impl PartialOrd for Node {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        other.distance.partial_cmp(&self.distance)
    }
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.distance.eq(&other.distance)
    }
}

impl Eq for Node {}

fn solve_part2(input: &Input) -> i32 {
    let grid = &input.hill;
    let starts = find_all_a(grid);
    let end = find_coords_of_flag(grid, Flag::End);

    starts.into_iter()
        .filter_map(|start| dijkstra(grid, start, end))
        .min()
        .unwrap()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_solve_part1_example() {
        let input = parse_input(include_str!("example"));
        let expected = 31;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1() {
        let input = parse_input(include_str!("input"));
        let expected = 447;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2_example() {
        let input = parse_input(include_str!("example"));
        let expected = 29;
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2() {
        let input = parse_input(include_str!("input"));
        let expected = 446;
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }
}
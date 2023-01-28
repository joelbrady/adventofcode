use std::collections::HashMap;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::line_ending;
use nom::combinator::map;
use nom::IResult;
use nom::multi::{count, many1, separated_list1};
use nom::sequence::separated_pair;

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
    grid: Vec<Vec<Tile>>,
    path: Vec<Instruction>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum Tile {
    NonExistent,
    Ground,
    Wall,
}

#[derive(Debug)]
enum Instruction {
    Turn(Direction),
    Walk(i64),
}

#[derive(Debug)]
enum Direction {
    Left,
    Right,
}

fn rotate_left((x, y): (i64, i64)) -> (i64, i64) {
    (-y, x)
}

fn rotate_right((x, y): (i64, i64)) -> (i64, i64) {
    (y, -x)
}

fn parse_input(s: &str) -> Input {
    let (_, (grid, path)) = separated_pair(parse_grid, count(line_ending, 2), parse_path)(s).unwrap();

    let grid = fill_in_grid(grid);

    Input { grid, path }
}

fn fill_in_grid(mut grid: Vec<Vec<Tile>>) -> Vec<Vec<Tile>> {
    let longest_row = grid.iter()
        .map(|row| row.len())
        .max()
        .unwrap();

    for row in &mut grid {
        for _ in 0..(longest_row - row.len() + 1) {
            row.push(Tile::NonExistent);
        }
    }

    grid
}

fn parse_grid(s: &str) -> IResult<&str, Vec<Vec<Tile>>> {
    map(
        separated_list1(line_ending, parse_row),
        |mut v| {
            v.insert(0, vec![Tile::NonExistent; v[0].len()]);
            v
        },
    )(s)
}

fn parse_row(s: &str) -> IResult<&str, Vec<Tile>> {
    map(
        many1(parse_tile),
        |mut v| {
            v.insert(0, Tile::NonExistent);
            v
        },
    )(s)
}

fn parse_tile(s: &str) -> IResult<&str, Tile> {
    alt((
        map(tag(" "), |_| Tile::NonExistent),
        map(tag("."), |_| Tile::Ground),
        map(tag("#"), |_| Tile::Wall),
    ))(s)
}

fn parse_path(s: &str) -> IResult<&str, Vec<Instruction>> {
    many1(parse_instruction)(s)
}

fn parse_instruction(s: &str) -> IResult<&str, Instruction> {
    alt((
        map(tag("R"), |_| Instruction::Turn(Direction::Right)),
        map(tag("L"), |_| Instruction::Turn(Direction::Left)),
        map(nom::character::complete::i64, Instruction::Walk),
    ))(s)
}

#[derive(Debug)]
struct Player {
    position: (i64, i64),
    direction: (i64, i64),
}

impl Player {
    fn password(&self) -> i64 {
        let (x, y) = self.position;

        let direction_score = match self.direction {
            (1, 0) => 0,
            (0, 1) => 1,
            (-1, 0) => 2,
            (0, -1) => 3,
            _ => panic!(),
        };

        1000 * y + 4 * x + direction_score
    }
}

fn solve_part1(input: &Input) -> i64 {
    let grid = &input.grid;

    let mut player = find_start_position(grid);
    let mut visited = HashMap::new();

    for instruction in &input.path {
        match instruction {
            Instruction::Turn(direction) => match direction {
                Direction::Left => player.direction = {
                    rotate_right(player.direction)
                },
                Direction::Right => {
                    player.direction = rotate_left(player.direction)
                },
            }
            Instruction::Walk(steps) => {
                for _ in 0..*steps {
                    let (mut x, mut y) = add(player.position, player.direction);

                    let min_y = (0..grid.len())
                        .find(|y| grid[*y][player.position.0 as usize] != Tile::NonExistent)
                        .unwrap() as i64;

                    let max_y = (0..grid.len()).rev()
                        .find(|y| {
                            grid[*y][player.position.0 as usize] != Tile::NonExistent
                        })
                        .unwrap() as i64;

                    let row = &grid[player.position.1 as usize];
                    let min_x = (0..row.len())
                        .find(|x| grid[player.position.1 as usize][*x] != Tile::NonExistent)
                        .unwrap() as i64;

                    let max_x = (0..row.len()).rev()
                        .find(|x| grid[player.position.1 as usize][*x] != Tile::NonExistent)
                        .unwrap() as i64;

                    if player.direction.0 != 0 {
                        if x < min_x {
                            x = (row.len() - 1) as i64;
                            while row[x as usize] == Tile::NonExistent && x != player.position.0 {
                                x -= 1;
                            }
                        } else if x > max_x {
                            x = min_x;
                            while row[x as usize] == Tile::NonExistent && x != player.position.0 {
                                x += 1;
                            }
                        }
                    } else if player.direction.1 != 0 {
                        if y < min_y {
                            y = max_y;
                            while grid[y as usize][x as usize] == Tile::NonExistent && y != player.position.1 {
                                y -= 1;
                            }
                        } else if y > max_y {
                            y = min_y;
                            while grid[y as usize][x as usize] == Tile::NonExistent && y != player.position.1 {
                                y += 1;
                            }
                        }
                    } else {
                        panic!("direction is zero");
                    }

                    // stopped by walls
                    if grid[y as usize][x as usize] == Tile::Wall {
                        continue;
                    }

                    if grid[y as usize][x as usize] == Tile::Ground {
                        if (x, y) != player.position {
                            visited.insert(player.position, player.direction);
                        }
                        player.position = (x, y);
                    }
                }
            }
        }

        // dbg!(&player);
        // print(grid, &visited);
    }

    player.password()
}

#[allow(dead_code)]
fn print(grid: &[Vec<Tile>], visited: &HashMap<(i64, i64), (i64, i64)>) {
    for y in 0..grid.len() {
        for x in 0..grid[y].len() {
            let key = &(x as i64, y as i64);
            match visited.get(key) {
                None => {
                    match grid[y][x] {
                        Tile::NonExistent => print!(" "),
                        Tile::Ground => print!("."),
                        Tile::Wall => print!("#"),
                    }
                }
                Some(direction) => match direction {
                    (1, 0) => print!(">"),
                    (0, 1) => print!("v"),
                    (-1, 0) => print!("<"),
                    (0, -1) => print!("^"),
                    _ => panic!(),
                }
            }
        }
        println!();
    }
    println!();
}

fn add(a: (i64, i64), b: (i64, i64)) -> (i64, i64) {
    (a.0 + b.0, (a.1 + b.1))
}

fn find_start_position(grid: &[Vec<Tile>]) -> Player {
    for x in 0..grid[1].len() {
        match grid[1][x] {
            Tile::NonExistent => {}
            Tile::Ground => {
                let player = Player {
                    position: (x as i64, 1),
                    direction: (1, 0),
                };

                return player;
            }
            Tile::Wall => {}
        }
    }

    unreachable!()
}


fn solve_part2(_input: &Input) -> i64 {
    todo!()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_solve_part1_example() {
        let input = parse_input(include_str!("example"));
        let expected = 6032;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1() {
        let input = parse_input(include_str!("input"));
        let expected = 60362;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2_example() {
        let input = parse_input(include_str!("example"));
        let expected = 0;
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2() {
        let input = parse_input(include_str!("input"));
        let expected = 0;
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_rev_range() {
        let expected = vec![3, 2, 1];
        let actual: Vec<_> = (1..4).rev().collect();

        assert_eq!(actual, expected)
    }
}
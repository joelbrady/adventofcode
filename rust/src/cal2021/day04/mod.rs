use std::collections::HashSet;

use nom::bytes::complete::tag;
use nom::character::complete::{multispace0, multispace1};
use nom::IResult;
use nom::multi::{count, many1, separated_list1};
use nom::sequence::{delimited, separated_pair};

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
    numbers: Vec<i64>,
    boards: Vec<Board>,
}

const BOARD_SIZE: usize = 5;

#[derive(Debug)]
struct Board {
    numbers: [[i64; BOARD_SIZE]; BOARD_SIZE],
}

impl Board {
    fn has_bingo(&self, drawn_numbers: &[i64]) -> bool {
        let drawn_numbers: HashSet<i64> = drawn_numbers.iter().copied().collect();
        for row in self.numbers {
            let set: HashSet<i64> = row.iter().copied().collect();
            if set.intersection(&drawn_numbers).count() == set.len() {
                return true
            }
        }

        for col in 0..BOARD_SIZE {
            let set: HashSet<i64> = [
                self.numbers[0][col],
                self.numbers[1][col],
                self.numbers[2][col],
                self.numbers[3][col],
                self.numbers[4][col]
            ].into_iter().collect();
            if set.intersection(&drawn_numbers).count() == set.len() {
                return true
            }
        }

        false
    }

    fn unmarked_numbers(&self, drawn_numbers: &[i64]) -> Vec<i64> {
        let drawn_numbers: HashSet<i64> = drawn_numbers.iter().copied().collect();
        self.numbers.iter()
            .flat_map(|row| row.iter())
            .filter(|n| !drawn_numbers.contains(n))
            .copied()
            .collect()
    }
}

fn parse_input(s: &str) -> Input {
    let (_, (numbers, boards)) = separated_pair(
        parse_numbers,
        multispace1,
        parse_boards,
    )(s).unwrap();

    Input {
        numbers,
        boards,
    }
}

fn parse_numbers(s: &str) -> IResult<&str, Vec<i64>> {
    separated_list1(tag(","), nom::character::complete::i64)(s)
}

fn parse_boards(s: &str) -> IResult<&str, Vec<Board>> {
    many1(parse_board)(s)
}

fn parse_board(s: &str) -> IResult<&str, Board> {
    let (r, numbers) = count(parse_board_number, BOARD_SIZE * BOARD_SIZE)(s)?;

    let mut board_array: [[i64; BOARD_SIZE]; BOARD_SIZE] = Default::default();

    let mut i = 0;
    for row in 0..BOARD_SIZE {
        for col in 0..BOARD_SIZE {
            board_array[row][col] = numbers[i];
            i += 1;
        }
    }

    let board = Board {
        numbers: board_array,
    };

    Ok((r, board))
}

fn parse_board_number(s: &str) -> IResult<&str, i64> {
    delimited(
        multispace0,
        nom::character::complete::i64,
        multispace0,
    )(s)
}

fn solve(input: &Input) -> i64 {
    let numbers_to_draw = &input.numbers;
    let boards = &input.boards;

    for i in 0..numbers_to_draw.len() {
        let slice = &numbers_to_draw[0..i];

        let boards_with_bingo: Vec<&Board> = boards.iter()
            .filter(|b| b.has_bingo(slice))
            .collect();

        if !boards_with_bingo.is_empty() {
            assert_eq!(boards_with_bingo.len(), 1);
            let winning_board = boards_with_bingo[0];
            let unmarked_numbers: Vec<i64> = winning_board.unmarked_numbers(slice);
            let sum: i64 = unmarked_numbers.iter().sum();
            let number_just_called = slice.last().unwrap();
            return sum * number_just_called;
        }
    }

    0
}

fn solve2(_: &Input) -> i64 {
    todo!()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_numbers() {
        let input = "1,2,3,4";
        let expected = vec![1, 2, 3, 4];

        let (r, actual) = parse_numbers(input).unwrap();

        assert_eq!(r, "");
        assert_eq!(actual, expected)
    }

    #[test]
    fn test_part1_example1() {
        let input = include_str!("example1");
        let input = parse_input(input);
        let expected = 4512;

        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 60368;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }
}
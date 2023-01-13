use std::cmp::Ordering;
use std::fmt::Formatter;

use itertools::Itertools;
use nom::{IResult, Parser};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::line_ending;
use nom::multi::{separated_list0, separated_list1};
use nom::sequence::{delimited, separated_pair, terminated};

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
    pairs: Vec<(Packet, Packet)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Packet(Vec<Value>);

#[derive(Debug, Clone, PartialEq, Eq)]
enum Value {
    Integer(i32),
    List(Vec<Value>),
}

fn parse_input(s: &str) -> Input {
    let (_, pairs) = separated_list1(line_ending, parse_pair)(s).unwrap();

    Input { pairs }
}

fn parse_pair(s: &str) -> IResult<&str, (Packet, Packet)> {
    let p = separated_pair(parse_packet, line_ending, parse_packet);
    terminated(p, line_ending)(s)
}

fn parse_packet(s: &str) -> IResult<&str, Packet> {
    let (rem, list) = parse_list(s)?;

    Ok((rem, Packet(list)))
}

fn parse_list(s: &str) -> IResult<&str, Vec<Value>> {
    let items = separated_list0(tag(","), parse_value);
    delimited(tag("["), items, tag("]"))(s)
}

fn parse_value(s: &str) -> IResult<&str, Value> {
    alt((
        nom::character::complete::i32.map(Value::Integer),
        parse_list.map(Value::List),
    ))(s)
}

fn solve_part1(input: &Input) -> i64 {
    input.pairs.iter()
        .enumerate()
        .filter_map(|(i, (a, b))| match in_order(&a.0, &b.0) {
            OrderResult::InOrder => Some(i as i64 + 1),
            OrderResult::OutOfOrder => None,
            OrderResult::Inconclusive => {
                dbg!(i, a, b);
                panic!()
            }
        })
        .sum()
}

#[derive(Debug, Eq, PartialEq)]
enum OrderResult {
    InOrder,
    OutOfOrder,
    Inconclusive,
}

fn in_order(a: &[Value], b: &[Value]) -> OrderResult {
    for i in 0..a.len() {
        if i >= b.len() {
            // a has more items than b
            return OrderResult::OutOfOrder;
        } else {
            let aa = &a[i];
            let bb = &b[i];

            let r = match aa {
                Value::Integer(an) => match bb {
                    Value::Integer(bn) => match an.cmp(bn) {
                        Ordering::Less => OrderResult::InOrder,
                        Ordering::Equal => OrderResult::Inconclusive,
                        Ordering::Greater => OrderResult::OutOfOrder,
                    }
                    Value::List(bl) => {
                        let al = std::slice::from_ref(aa);
                        in_order(al, bl)
                    }
                }
                Value::List(al) => match bb {
                    Value::Integer(_) => {
                        let bl = std::slice::from_ref(bb);
                        in_order(al, bl)
                    }
                    Value::List(bl) => in_order(al, bl),
                },
            };

            match r {
                OrderResult::InOrder => return OrderResult::InOrder,
                OrderResult::OutOfOrder => return OrderResult::OutOfOrder,
                OrderResult::Inconclusive => {}
            }
        }
    }

    if a.len() < b.len() {
        // left side ran out of items
        OrderResult::InOrder
    } else {
        assert_eq!(a.len(), b.len());
        OrderResult::Inconclusive
    }
}

struct FVs<'a>(&'a [Value]);

impl <'a> std::fmt::Display for FVs<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        let s = self.0.iter()
            .map(|v| match v {
                Value::Integer(n) => format!("{n}"),
                Value::List(l) => format!("{}", FVs(l)),
            })
            .join(",");
        write!(f, "{s}")?;
        write!(f, "]")?;

        Ok(())
    }
}

fn solve_part2(input: &Input) -> i64 {
    let divider_packets = [parse_packet("[[2]]").unwrap().1, parse_packet("[[6]]").unwrap().1];

    let sorted_packets: Vec<Packet> = input.pairs.iter()
        .cloned()
        .flat_map(|(a, b)| [a, b].into_iter())
        .chain(divider_packets.iter().cloned())
        .sorted_by(|a, b| {
            match in_order(&a.0, &b.0) {
                OrderResult::InOrder => Ordering::Less,
                OrderResult::OutOfOrder => Ordering::Greater,
                OrderResult::Inconclusive => panic!(),
            }
        })
        .collect();

    sorted_packets.iter()
        .enumerate()
        .map(|(i, item)| ((i + 1) as i64, item))
        .filter(|(_, packet)| divider_packets.contains(packet))
        .map(|(i, _)| i)
        .product()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_in_order_example1() {
        let (_, a) = parse_packet("[1,1,3,1,1]").unwrap();
        let (_, b) = parse_packet("[1,1,5,1,1]").unwrap();

        assert_eq!(in_order(&a.0, &b.0), OrderResult::InOrder)
    }

    #[test]
    fn test_in_order_example2() {
        let (_, a) = parse_packet("[[1],[2,3,4]]").unwrap();
        let (_, b) = parse_packet("[[1],4]").unwrap();

        assert_eq!(in_order(&a.0, &b.0), OrderResult::InOrder)
    }

    #[test]
    fn test_in_order_example3() {
        let (_, a) = parse_packet("[9]").unwrap();
        let (_, b) = parse_packet("[[8,7,6]]").unwrap();

        assert_eq!(in_order(&a.0, &b.0), OrderResult::OutOfOrder)
    }

    #[test]
    fn test_in_order_example4() {
        let (_, a) = parse_packet("[[4,4],4,4]").unwrap();
        let (_, b) = parse_packet("[[4,4],4,4,4]").unwrap();

        assert_eq!(in_order(&a.0, &b.0), OrderResult::InOrder)
    }

    #[test]
    fn test_in_order_example5() {
        let (_, a) = parse_packet("[7,7,7,7]").unwrap();
        let (_, b) = parse_packet("[7,7,7]").unwrap();

        assert_eq!(in_order(&a.0, &b.0), OrderResult::OutOfOrder)
    }

    #[test]
    fn test_in_order_example6() {
        let (_, a) = parse_packet("[]").unwrap();
        let (_, b) = parse_packet("[3]").unwrap();

        assert_eq!(in_order(&a.0, &b.0), OrderResult::InOrder)
    }

    #[test]
    fn test_in_order_example7() {
        let (_, a) = parse_packet("[[[]]]").unwrap();
        let (_, b) = parse_packet("[[]]").unwrap();

        assert_eq!(in_order(&a.0, &b.0), OrderResult::OutOfOrder)
    }

    #[test]
    fn test_in_order_example8() {
        let (_, a) = parse_packet("[1,[2,[3,[4,[5,6,7]]]],8,9]").unwrap();
        let (_, b) = parse_packet("[1,[2,[3,[4,[5,6,0]]]],8,9]").unwrap();

        assert_eq!(in_order(&a.0, &b.0), OrderResult::OutOfOrder)
    }

    #[test]
    fn test_solve_part1_example() {
        let input = parse_input(include_str!("example"));
        let expected = 13;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1() {
        let input = parse_input(include_str!("input"));
        let expected = 5843;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2_example() {
        let input = parse_input(include_str!("example"));
        let expected = 140;
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2() {
        let input = parse_input(include_str!("input"));
        let expected = 26289;
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }
}
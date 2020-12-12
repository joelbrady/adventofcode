pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let part1 = solve(&input);

    println!("The solution to part 1 is {}", part1);

    // let part2 = solve2(&input);
    // println!("The solution to part 2 is {}", part2);
}

#[derive(Debug)]
enum Instruction {
    Forward(u32),
    North(u32),
    South(u32),
    East(u32),
    West(u32),
    Left(u32),
    Right(u32),
}

fn parse_input(input: &str) -> Vec<Instruction> {
    input.lines()
        .map(|s| {
            let label = &s[0..1];
            let n = &s[1..];
            let n: u32 = n.parse().unwrap();
            match label {
                "F" => Instruction::Forward(n),
                "N" => Instruction::North(n),
                "S" => Instruction::South(n),
                "E" => Instruction::East(n),
                "W" => Instruction::West(n),
                "L" => Instruction::Left(n),
                "R" => Instruction::Right(n),
                _ => unimplemented!(),
            }
        })
        .collect()
}

fn solve(input: &[Instruction]) -> i32 {
    let mut direction = (1, 0);
    let mut location = (0, 0);

    for i in input.iter() {
        match i {
            Instruction::Forward(n) => {
                for _ in 0..*n {
                    location = add(&location, &direction);
                }
            }
            Instruction::North(n) => {
                for _ in 0..*n {
                    location = add(&location, &(0, 1));
                }
            }
            Instruction::South(n) => {
                for _ in 0..*n {
                    location = add(&location, &(0, -1));
                }
            }
            Instruction::East(n) => {
                for _ in 0..*n {
                    location = add(&location, &(1, 0));
                }
            }
            Instruction::West(n) => {
                for _ in 0..*n {
                    location = add(&location, &(-1, 0));
                }
            }
            Instruction::Left(n) => {
                let mut n = *n;
                while n > 0 {
                    match direction {
                        (1, 0) => {
                            direction = (0, 1);
                        },
                        (0, 1) => {
                            direction = (-1, 0);
                        },
                        (-1, 0) => {
                            direction = (0, -1);
                        },
                        (0, -1) => {
                            direction = (1, 0);
                        },
                        _ => unimplemented!(),
                    }
                    n -= 90;
                }
            },
            Instruction::Right(n) => {
                let mut n = *n;
                while n > 0 {
                    match direction {
                        (1, 0) => {
                            direction = (0, -1);
                        },
                        (0, 1) => {
                            direction = (1, 0);
                        },
                        (-1, 0) => {
                            direction = (0, 1);
                        },
                        (0, -1) => {
                            direction = (-1, 0);
                        },
                        _ => unimplemented!(),
                    }
                    n -= 90;
                }
            }
        }
    }

    abs(location.0) + abs(location.1)
}

fn abs(n: i32) -> i32 {
    if n < 0 {
        -n
    } else {
        n
    }
}

fn add((ax, ay): &(i32, i32), (bx, by): &(i32, i32)) -> (i32, i32) {
    (ax + bx, ay + by)
}

// fn solve2(input: &[Instruction]) -> i32 {
//     unimplemented!()
// }

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_example() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = 25;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 1687;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }
}
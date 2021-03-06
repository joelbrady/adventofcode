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
    instructions: Vec<Instruction>,
}

#[derive(Debug)]
enum Instruction {
    Forward(i64),
    Down(i64),
    Up(i64),
}

fn parse_input(s: &str) -> Input {
    let instructions = s.lines()
        .map(|l| parse_instruction(l))
        .collect();

    Input {
        instructions
    }
}

fn parse_instruction(s: &str) -> Instruction {
    let split: Vec<&str> = s.split(' ').collect();

    let action = split[0];
    let amount: i64 = split[1].parse().unwrap();

    if action == "forward" {
        Instruction::Forward(amount)
    } else if action == "down" {
        Instruction::Down(amount)
    } else if action == "up" {
        Instruction::Up(amount)
    } else {
        panic!()
    }
}

struct Location {
    depth: i64,
    distance: i64,
    aim: i64,
}

impl Location {
    fn part2_forward(&self, x: i64) -> Location {
        Location {
            distance: self.distance + x,
            depth: self.depth + (self.aim * x),
            ..*self
        }
    }

    fn increase_aim(&self, x: i64) -> Location {
        Location {
            aim: self.aim + x,
            ..*self
        }
    }

    fn down(&self, amount: i64) -> Location {
        Location {
            depth: self.depth + amount,
            ..*self
        }
    }

    fn up(&self, amount: i64) -> Location {
        Location {
            depth: self.depth - amount,
            ..*self
        }
    }

    fn forward(&self, amount: i64) -> Location {
        Location {
            distance: self.distance + amount,
            ..*self
        }
    }
}

impl Default for Location {
    fn default() -> Self {
        Location {
            depth: 0,
            distance: 0,
            aim: 0,
        }
    }
}

fn solve(input: &Input) -> i64 {
    let location = input.instructions.iter()
        .fold(Location::default(), |acc, i| {
            match i {
                Instruction::Forward(n) => acc.forward(*n),
                Instruction::Down(n) => acc.down(*n),
                Instruction::Up(n) => acc.up(*n),
            }
        });

    location.depth * location.distance
}

fn solve2(input: &Input) -> i64 {
    let location = input.instructions.iter()
        .fold(Location::default(), |acc, i| {
            match i {
                Instruction::Forward(x) => acc.part2_forward(*x),
                Instruction::Down(x) => acc.increase_aim(*x),
                Instruction::Up(x) => acc.increase_aim(-*x),
            }
        });

    location.depth * location.distance
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_part1_example1() {
        let input = include_str!("example1");
        let input = parse_input(input);
        let expected = 150;

        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 2073315;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_part2_example1() {
        let input = include_str!("example1");
        let input = parse_input(input);
        let expected = 900;

        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 1840311528;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }
}
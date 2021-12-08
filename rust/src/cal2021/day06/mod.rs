use nom::bytes::complete::tag;
use nom::IResult;
use nom::multi::separated_list1;

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
    fish: Vec<u64>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
struct State {
    fish: [u64; 9],
}

impl State {
    fn new(input: &[u64]) -> State {
        let mut fish = [0u64; 9];

        input.iter().for_each(|c| {
            fish[*c as usize] += 1;
        });

        State { fish }
    }

    fn next(&self) -> State {
        let mut new_state = [0u64; 9];

        new_state[8] = self.fish[0];
        new_state[7] = self.fish[8];
        new_state[6] = self.fish[7] + self.fish[0];
        new_state[5] = self.fish[6];
        new_state[4] = self.fish[5];
        new_state[3] = self.fish[4];
        new_state[2] = self.fish[3];
        new_state[1] = self.fish[2];
        new_state[0] = self.fish[1];

        State {
            fish: new_state,
        }
    }

    fn total_fish(&self) -> u64 {
        self.fish.iter().sum()
    }
}

fn parse_input(s: &str) -> Input {
    let result: IResult<&str, Vec<u64>> = separated_list1(
        tag(","),
        nom::character::complete::u64,
    )(s);

    let (_, fish) = result.unwrap();

    Input { fish }
}

fn solve(input: &Input) -> u64 {
    let initial_state = State::new(&input.fish);

    let final_state = iterate(&initial_state, 80);

    final_state.total_fish()
}

fn iterate(fish: &State, iterations: usize) -> State {
    let mut new_state = fish.clone();
    for _ in 0..iterations {
        new_state = new_state.next();
    }

    new_state
}

fn solve2(input: &Input) -> u64 {
    let initial_state = State::new(&input.fish);

    let final_state = iterate(&initial_state, 256);

    final_state.total_fish()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_part1_1_iterations() {
        let start = State { fish: [0, 1, 0, 0, 0, 0, 0, 0, 0] };
        let expected = State { fish: [1, 0, 0, 0, 0, 0, 0, 0, 0] };

        let actual = iterate(&start, 1);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_part1_1_iterations2() {
        let start = State { fish: [1, 1, 0, 0, 0, 0, 0, 0, 0] };
        let expected = State { fish: [1, 0, 0, 0, 0, 0, 1, 0, 1] };

        let actual = iterate(&start, 1);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_part1_example1_18_iterations() {
        let input = include_str!("example1");
        let input = parse_input(input);
        let expected = 26;

        let actual = iterate(&State::new(&input.fish), 18);
        let actual: u64 = actual.total_fish();

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_part1_example1() {
        let input = include_str!("example1");
        let input = parse_input(input);
        let expected = 5934;

        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 390011;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_part2_example1() {
        let input = include_str!("example1");
        let input = parse_input(input);
        let expected = 26984457539;

        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 1746710169834;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }
}
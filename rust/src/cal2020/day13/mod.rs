pub fn main() {
    let input = include_str!("input");
    let input = parse_input(input);

    let part1 = solve(&input);

    println!("The solution to part 1 is {}", part1);

    // let part2 = solve2(&input);
    // println!("The solution to part 2 is {}", part2);
}

#[derive(Debug)]
struct Input {
    arrival_time: u32,
    buses: Vec<u32>,
}

fn parse_input(input: &str) -> Input {
    let lines: Vec<&str> = input.lines().collect();
    let arrival_time = lines[0].parse().unwrap();
    let buses = input.split(',')
        .filter_map(|bus| bus.parse().ok())
        .collect();

    Input {
        arrival_time,
        buses,
    }
}

fn solve(input: &Input) -> u32 {
    let (bus_id, closest_multiple) = input.buses.iter()
        .map(|bus_id| (*bus_id, f(*bus_id, input.arrival_time)))
        .min_by(|(_, a), (_, b)| a.cmp(b))
        .unwrap();

    bus_id * (closest_multiple - input.arrival_time)
}

fn f(bus_id: u32, arrival_time: u32) -> u32 {
    let mut n = bus_id;

    while n < arrival_time {
        n += bus_id;
    }

    n
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_example() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = 295;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 4782;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    // #[test]
    // fn test_example_part2() {
    //     let input = include_str!("example");
    //     let input = parse_input(input);
    //
    //     let expected = 286;
    //     let actual = solve2(&input);
    //
    //     assert_eq!(actual, expected)
    // }
    //
    // #[test]
    // fn test_solution_part2() {
    //     let input = include_str!("input");
    //     let input = parse_input(input);
    //
    //     let expected = 20873;
    //     let actual = solve2(&input);
    //
    //     assert_eq!(actual, expected)
    // }
}
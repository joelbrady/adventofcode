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
    arrival_time: u32,
    buses: Vec<(usize, u32)>,
}

fn parse_input(input: &str) -> Input {
    let lines: Vec<&str> = input.lines().collect();
    let arrival_time = lines[0].parse().unwrap();
    let buses = lines[1].split(',')
        .enumerate()
        .filter_map(|(index, bus)| bus.parse().ok().map(|b| (index, b)))
        .collect();

    Input {
        arrival_time,
        buses,
    }
}

fn solve(input: &Input) -> u32 {
    let (bus_id, closest_multiple) = input.buses.iter()
        .map(|(_, bus_id)| (*bus_id, f(*bus_id, input.arrival_time)))
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

#[derive(Debug)]
struct Bus {
    offset: usize,
    bus_id: u128,
}

#[derive(Debug)]
struct ModEquation {
    modulus: u128,
    rhs: u128,
}

impl ModEquation {
    fn is_congruent(&self, n: &u128) -> bool {
        (n % self.modulus) == self.rhs
    }
}

fn solve2(input: &Input) -> u128 {
    let v: Vec<Bus> = input.buses.iter()
        .map(|(index, bus_id)| Bus { offset: *index, bus_id: *bus_id as u128 })
        .collect();

    let mut equations: Vec<ModEquation> = v.iter()
        .map(|b| ModEquation { modulus: b.bus_id, rhs: calculate_rhs(b) })
        .collect();

    let mut equation = equations.remove(0);

    while !equations.is_empty() {
        let b = equations.remove(0);
        equation = solve_modular_equation(&equation, &b);
    }

    equation.rhs
}

fn calculate_rhs(bus: &Bus) -> u128 {
    let mut rhs: i128 = -(bus.offset as i128);

    let modulus = bus.bus_id as i128;

    while rhs < 0 {
        rhs += modulus;
    }

    rhs as u128
}

fn solve_modular_equation(a: &ModEquation, b: &ModEquation) -> ModEquation {
    let modulus = a.modulus * b.modulus;

    let mut n = a.rhs;

    while !b.is_congruent(&n) {
        n += a.modulus;
    }

    ModEquation { modulus, rhs: n }
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

    #[test]
    fn test_example_part2() {
        let input = include_str!("example");
        let input = parse_input(input);

        let expected = 1068781;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_example_small1() {
        let input = "1\n17,x,13,19";
        let input = parse_input(input);

        let expected = 3417;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_example_small2() {
        let input = "1\n67,7,59,61";
        let input = parse_input(input);

        let expected = 754018;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_example_small3() {
        let input = "1\n67,x,7,59,61";
        let input = parse_input(input);

        let expected = 779210;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_example_small4() {
        let input = "1\n67,7,x,59,61";
        let input = parse_input(input);

        let expected = 1261476;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_example_small5() {
        let input = "1\n1789,37,47,1889";
        let input = parse_input(input);

        let expected = 1202161486;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution_part2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 1118684865113056;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }
}

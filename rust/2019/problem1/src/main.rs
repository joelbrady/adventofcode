use std::io::BufRead;

fn main() {
    let input = get_input();
    let part1 = solve(&input, fuel_requirement);

    println!("The solution to part 1 is {}", part1);

    let part2 = solve(&input, part2_requirement);

    println!("The solution to part 2 is {}", part2);
}

fn get_input() -> Vec<String> {
    std::io::stdin()
        .lock()
        .lines()
        .map(|s| s.unwrap())
        .collect()
}

fn solve<F>(input: &Vec<String>, f: F) -> i32
    where F: Fn(i32) -> i32 {
    input.into_iter()
        .map(|s| s.parse::<i32>().unwrap())
        .map(f)
        .sum()
}

fn fuel_requirement(module: i32) -> i32 {
    // take its mass, divide by three, round down, and subtract 2.
    (module / 3) - 2
}

fn part2_requirement(module: i32) -> i32 {
    let base_cost = fuel_requirement(module);
    if base_cost > 0 {
        base_cost + part2_requirement(base_cost)
    } else {
        0
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_fuel_requirement() {
        assert_eq!(fuel_requirement(12), 2);
        assert_eq!(fuel_requirement(14), 2);
        assert_eq!(fuel_requirement(1969), 654);
        assert_eq!(fuel_requirement(100756), 33583);
    }

    #[test]
    fn test_part2_requirement() {
        assert_eq!(part2_requirement(14), 2);
        assert_eq!(part2_requirement(1969), 966);
        assert_eq!(part2_requirement(100756), 50346)
    }
}

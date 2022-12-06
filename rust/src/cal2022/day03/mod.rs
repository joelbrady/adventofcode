pub fn main() {
    let input = include_str!("input");

    let input = parse_input(input);

    let part1 = solve_part1(&input);

    println!("The solution to part 1 is {}", part1);

    let part2 = solve_part2(&input);
    println!("The solution to part 2 is {}", part2);
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct Input {
    rucksacks: Vec<Rucksack>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct Rucksack {
    contents: Vec<char>,
}

fn parse_input(s: &str) -> Input {
    let rucksacks = s.lines()
        .map(|l| Rucksack {
            contents: l.chars().collect()
        })
        .collect();
    Input { rucksacks }
}

fn solve_part1(input: &Input) -> u32 {
    input.rucksacks.iter()
        .map(|bag| {
            let (l, r) = bag.contents.split_at(bag.contents.len() / 2);
            let item = in_both(l, r);
            priority(item)
        })
        .sum()
}

fn in_both(left: &[char], right: &[char]) -> char {
    *left.iter()
        .find(|item| right.contains(item))
        .unwrap()
}

fn priority(item: char) -> u32 {
    let item = item as u32;
    if (65..=90).contains(&item) {
        item - 65 + 27
    } else if (97..=122).contains(&item) {
        item - 97 + 1
    } else {
        unimplemented!("{item}")
    }
}

fn solve_part2(_input: &Input) -> i64 {
    todo!()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_in_both() {
        let left = "vJrwpWtwJgWr".chars().collect::<Vec<_>>();
        let right = "hcsFMMfFFhFp".chars().collect::<Vec<_>>();
        let expected = 'p';
        let actual = in_both(&left, &right);
        assert_eq!(actual, expected)
    }

    #[test]
    fn test_priorities() {
        [
            ('a', 1),
            ('p', 16),
            ('L', 38),
            ('P', 42),
            ('v', 22),
            ('t', 20),
            ('s', 19),
        ].into_iter()
            .for_each(|(c, n)| assert_eq!(priority(c), n))
    }

    #[test]
    fn test_solve_part1_example() {
        let input = parse_input(include_str!("example"));
        let expected = 157;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1() {
        let input = parse_input(include_str!("input"));
        let expected = 8053;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }
}
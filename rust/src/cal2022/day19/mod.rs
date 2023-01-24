use std::collections::{HashSet, VecDeque};

use nom::bytes::complete::tag;
use nom::character::complete::line_ending;
use nom::IResult;
use nom::multi::separated_list1;

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
    blueprints: Vec<Blueprint>,
}

#[derive(Debug)]
struct Blueprint {
    id: i64,
    ore_robot_cost: Cost,
    clay_robot_cost: Cost,
    obsidian_robot_cost: Cost,
    geode_robot_cost: Cost,
}

impl Blueprint {
    fn sum_of_ore_costs(&self) -> i64 {
        self.ore_robot_cost.ore
            + self.clay_robot_cost.ore
            + self.obsidian_robot_cost.ore
            + self.geode_robot_cost.ore
    }

    fn sum_of_clay_costs(&self) -> i64 {
        self.ore_robot_cost.clay
            + self.clay_robot_cost.clay
            + self.obsidian_robot_cost.clay
            + self.geode_robot_cost.clay
    }

    fn sum_of_obsidian_costs(&self) -> i64 {
        self.ore_robot_cost.obsidian
            + self.clay_robot_cost.obsidian
            + self.obsidian_robot_cost.obsidian
            + self.geode_robot_cost.obsidian
    }
}

#[derive(Debug)]
struct Cost {
    ore: i64,
    clay: i64,
    obsidian: i64,
}

impl Cost {
    fn ore(amount: i64) -> Self {
        Self {
            ore: amount,
            clay: 0,
            obsidian: 0,
        }
    }
}

fn parse_input(s: &str) -> Input {
    let (_, blueprints) = separated_list1(line_ending, parse_blueprint)(s).unwrap();
    Input { blueprints }
}

fn parse_blueprint(s: &str) -> IResult<&str, Blueprint> {
    let int = nom::character::complete::i64;

    let (rem, _) = tag("Blueprint ")(s)?;
    let (rem, id) = int(rem)?;
    let (rem, _) = tag(": Each ore robot costs ")(rem)?;
    let (rem, ore_robot_cost) = int(rem)?;
    let (rem, _) = tag(" ore. Each clay robot costs ")(rem)?;
    let (rem, clay_robot_cost) = int(rem)?;
    let (rem, _) = tag(" ore. Each obsidian robot costs ")(rem)?;
    let (rem, obsidian_robot_ore) = int(rem)?;
    let (rem, _) = tag(" ore and ")(rem)?;
    let (rem, obsidian_robot_clay) = int(rem)?;
    let (rem, _) = tag(" clay. Each geode robot costs ")(rem)?;
    let (rem, geode_robot_ore) = int(rem)?;
    let (rem, _) = tag(" ore and ")(rem)?;
    let (rem, geode_robot_obsidian) = int(rem)?;
    let (rem, _) = tag(" obsidian.")(rem)?;

    let blueprint = Blueprint {
        id,
        ore_robot_cost: Cost::ore(ore_robot_cost),
        clay_robot_cost: Cost::ore(clay_robot_cost),
        obsidian_robot_cost: Cost { ore: obsidian_robot_ore, clay: obsidian_robot_clay, obsidian: 0 },
        geode_robot_cost: Cost { ore: geode_robot_ore, obsidian: geode_robot_obsidian, clay: 0 },
    };

    Ok((rem, blueprint))
}

#[derive(Debug, Hash, PartialEq, Eq)]
struct Node {
    steps: i64,
    ore: i64,
    clay: i64,
    obsidian: i64,
    geode: i64,

    ore_robots: i64,
    clay_robots: i64,
    obsidian_robots: i64,
    geode_robots: i64,
}

impl Node {
    fn child_with<F>(&self, f: F) -> Self
        where F: FnOnce(&mut Self) {
        let mut new = Self {
            steps: self.steps + 1,
            ore: self.ore,
            clay: self.clay,
            obsidian: self.obsidian,
            geode: self.geode,

            ore_robots: self.ore_robots,
            clay_robots: self.clay_robots,
            obsidian_robots: self.obsidian_robots,
            geode_robots: self.geode_robots,
        };

        f(&mut new);

        new
    }

    fn can_afford(&self, cost: &Cost) -> bool {
        self.ore >= cost.ore && self.clay >= cost.clay && self.obsidian >= cost.obsidian
    }

    fn subtract(&mut self, cost: &Cost) {
        self.ore -= cost.ore;
        self.clay -= cost.clay;
        self.obsidian -= cost.obsidian;
    }

    fn add_robot(&mut self, robot_type: RobotType) {
        match robot_type {
            RobotType::Ore => self.ore_robots += 1,
            RobotType::Clay => self.clay_robots += 1,
            RobotType::Obsidian => self.obsidian_robots += 1,
            RobotType::Geode => self.geode_robots += 1,
        }
    }

    fn generate_resources(&mut self) {
        self.ore += self.ore_robots;
        self.clay += self.clay_robots;
        self.obsidian += self.obsidian_robots;
        self.geode += self.geode_robots;
    }

    fn has_enough(&self, blueprint: &Blueprint, robot_type: RobotType) -> bool {
        match robot_type {
            RobotType::Ore => self.ore_robots >= blueprint.sum_of_ore_costs(),
            RobotType::Clay => self.clay_robots >= blueprint.sum_of_clay_costs(),
            RobotType::Obsidian => self.obsidian_robots >= blueprint.sum_of_obsidian_costs(),
            RobotType::Geode => false,
        }
    }
}

fn solve_part1(input: &Input) -> i64 {
    input.blueprints.iter()
        .map(|b| {
            let geodes = simulate(b);
            b.id * geodes
        })
        .sum()
}

fn simulate(blueprint: &Blueprint) -> i64 {
    let mut queue = VecDeque::new();

    let start_node = Node {
        steps: 0,
        ore: 0,
        clay: 0,
        obsidian: 0,
        geode: 0,

        ore_robots: 1,
        clay_robots: 0,
        obsidian_robots: 0,
        geode_robots: 0,
    };

    queue.push_front(start_node);

    let mut best = 0;
    let mut seen = HashSet::new();

    while let Some(current) = queue.pop_back() {
        if seen.contains(&current) {
            continue;
        }

        if current.steps == 24 {
            best = best.max(current.geode);
            continue;
        }

        assert!(current.steps < 24);

        let children = generate_children(blueprint, &current, best);
        queue.extend(children);
        seen.insert(current);
    }

    best
}

#[derive(Debug, Copy, Clone)]
enum RobotType {
    Ore,
    Clay,
    Obsidian,
    Geode,
}

fn generate_children(blueprint: &Blueprint, current: &Node, current_best: i64) -> Vec<Node> {
    let mut children = vec![];

    for (cost, robot_type) in [
        (&blueprint.ore_robot_cost, RobotType::Ore),
        (&blueprint.clay_robot_cost, RobotType::Clay),
        (&blueprint.obsidian_robot_cost, RobotType::Obsidian),
        (&blueprint.geode_robot_cost, RobotType::Geode),
    ] {
        let remaining = 24 - current.steps;
        if current.geode + remaining * current.geode_robots + triangle(remaining) > current_best {
            if current.can_afford(cost) && !current.has_enough(blueprint, robot_type) {
                let node = current.child_with(|n| {
                    n.subtract(cost);
                    n.generate_resources();
                    n.add_robot(robot_type);
                });

                children.push(node);
            } else {
                let node = current.child_with(|n| {
                    n.generate_resources();
                });

                children.push(node);
            }
        }
    }

    children
}

fn triangle(n: i64) -> i64 {
    (n * (n - 1)) / 2
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
        let expected = 33;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1() {
        let input = parse_input(include_str!("input"));
        let expected = 2301;
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
}
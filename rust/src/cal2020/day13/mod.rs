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
    multiple: u128,
}

fn solve2(input: &Input) -> u128 {
    let mut v: Vec<Bus> = input.buses.iter()
        .map(|(index, bus_id)| Bus { offset: *index, bus_id: *bus_id as u128, multiple: *bus_id as u128 })
        .collect();

    let index_of_largest_bus_id = v.iter().enumerate().max_by_key(|(_, b)| b.bus_id).unwrap().0;
    let largest_bus_id = v.iter().max_by_key(|b| b.bus_id).unwrap().bus_id;

    dbg!(index_of_largest_bus_id);
    dbg!(largest_bus_id);

    let mut i = 0;

    while !check(&v) {
        v[index_of_largest_bus_id].multiple += largest_bus_id;
        let target = v[index_of_largest_bus_id].multiple;
        bring_everyone_close_to_largest_bus_multiple(&mut v, target);
        // 3417
        // if i >= 178 {
        //     dbg!(&v);
        //     panic!();
        // }
        // i += 1;
    }

    v[0].multiple
}

fn bring_everyone_close_to_largest_bus_multiple(v: &mut [Bus], largest_bus_id_multiple: u128) {
    for b in v.iter_mut() {
        if b.multiple != largest_bus_id_multiple {
            let mut a = largest_bus_id_multiple - (largest_bus_id_multiple % b.bus_id) + b.bus_id;
            while a > largest_bus_id_multiple {
                a -= b.bus_id;
            }
            b.multiple = a;
        }
    }
}

fn check(v: &[Bus]) -> bool {
    let first = v[0].multiple;
    for b in v.iter() {
        if b.multiple != (first + (b.offset as u128)) {
            return false;
        }
    }

    true
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
    fn test_check_example() {
        let v = vec![
            Bus {
                bus_id: 7,
                multiple: 1068781,
                offset: 0,
            },
            Bus {
                bus_id: 13,
                multiple: 1068782,
                offset: 1,
            },
            Bus {
                bus_id: 59,
                multiple: 1068785,
                offset: 4,
            },
            Bus {
                bus_id: 31,
                multiple: 1068787,
                offset: 6,
            },
            Bus {
                bus_id: 19,
                multiple: 1068788,
                offset: 7,
            },
        ];

        assert!(check(&v))
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
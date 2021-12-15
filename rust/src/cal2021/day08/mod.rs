use std::collections::{BTreeMap, BTreeSet};

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
    entries: Vec<Entry>,
}

#[derive(Debug)]
struct Entry {
    signal_patterns: Vec<Pattern>,
    output_value: Vec<Pattern>,
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone)]
struct Pattern(BTreeSet<Segment>);

impl Pattern {
    fn new(segments: &[Segment]) -> Pattern {
        Pattern(segments.iter().copied().collect())
    }

    fn minus(&self, other: &Pattern) -> Vec<Segment> {
        ((&self.0) - (&other.0)).into_iter().collect()
    }

    fn without(&self, not_this: &[Segment]) -> Vec<Segment> {
        self.0.iter()
            .filter(|s| !not_this.contains(*s))
            .copied()
            .collect()
    }
}

impl From<Vec<Segment>> for Pattern {
    fn from(segments: Vec<Segment>) -> Self {
        Pattern::new(&segments)
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, Ord, PartialOrd)]
enum Segment {
    A,
    B,
    C,
    D,
    E,
    F,
    G,
}

impl Segment {
    fn values() -> impl Iterator<Item = Segment> {
        use Segment::*;

        [A, B, C, D, E, F, G].into_iter()
    }
}

fn parse_input(s: &str) -> Input {
    let entries = s.lines()
        .map(parse_entry)
        .collect();

    Input {
        entries,
    }
}

fn parse_entry(s: &str) -> Entry {
    let ss: Vec<&str> = s.split(" | ").collect();

    let signal_patterns = parse_patterns(ss[0]);
    let output_value = parse_patterns(ss[1]);

    Entry {
        signal_patterns,
        output_value,
    }
}

fn parse_patterns(s: &str) -> Vec<Pattern> {
    s.split(' ')
        .map(parse_pattern)
        .collect()
}

fn parse_pattern(s: &str) -> Pattern {
    use Segment::*;

    let set = s.chars()
        .map(|c| match c {
            'a' => A,
            'b' => B,
            'c' => C,
            'd' => D,
            'e' => E,
            'f' => F,
            'g' => G,
            a => panic!("{:?}", a),
        })
        .collect();

    Pattern(set)
}

fn solve(input: &Input) -> usize {
    input.entries.iter()
        .flat_map(|entry| entry.output_value.iter())
        .filter(|pattern| is_unique_pattern(pattern))
        .count()
}

fn is_unique_pattern(p: &Pattern) -> bool {
    // 1, 4, 7, 8
    matches!(p.0.len(), 2 | 4 | 3 | 7)
}

fn solve2(input: &Input) -> i64 {
    input.entries.iter()
        .map(|e| {
            let lookup_table = build_lookup_table(e);
            let n: String = e.output_value.iter()
                .map(|digit| *lookup_table.get(digit).unwrap())
                .collect();

            let n: i64 = n.parse().unwrap();
            n
        })
        .sum()
}

fn build_lookup_table(entry: &Entry) -> BTreeMap<Pattern, &'static str> {
    let patterns: &[Pattern] = &entry.signal_patterns;

    let one = match_len(patterns, 2).clone();
    let four = match_len(patterns, 4).clone();
    let seven = match_len(patterns, 3).clone();
    let eight = match_len(patterns, 7).clone();

    let top_segment = seven.minus(&one)[0];
    let top_right_segment = find_segments_that_occur_n_times(patterns, 8)
        .into_iter()
        .find(|s| *s != top_segment)
        .unwrap();

    let bottom_right_segment = find_segments_that_occur_n_times(patterns, 9)[0];
    let bottom_left_segment = find_segments_that_occur_n_times(patterns, 4)[0];
    let top_left_segment = find_segments_that_occur_n_times(patterns, 6)[0];
    let middle_segment = find_segments_that_occur_n_times(patterns, 7)
        .into_iter()
        .find(|s| four.0.contains(s))
        .unwrap();

    let zero: Pattern = eight.without(&[middle_segment]).into();
    let two: Pattern = eight.without(&[top_left_segment, bottom_right_segment]).into();
    let three: Pattern = eight.without(&[top_left_segment, bottom_left_segment]).into();
    let five: Pattern = eight.without(&[top_right_segment, bottom_left_segment]).into();
    let six: Pattern = eight.without(&[top_right_segment]).into();
    let nine: Pattern = eight.without(&[bottom_left_segment]).into();

    [
        (zero, "0"),
        (one, "1"),
        (two, "2"),
        (three, "3"),
        (four, "4"),
        (five, "5"),
        (six, "6"),
        (seven, "7"),
        (eight, "8"),
        (nine, "9"),
    ]
        .into_iter()
        .collect()
}

fn find_segments_that_occur_n_times(all_patterns: &[Pattern], n: usize) -> Vec<Segment> {
    Segment::values()
        .map(|s| {
            let n = all_patterns.iter()
                .filter(|p| p.0.contains(&s))
                .count();

            (s, n)
        })
        .filter(|(_, count)| *count == n)
        .map(|(s, _)| s)
        .collect()
}

fn match_len(patterns: &[Pattern], len: usize) -> &Pattern {
    patterns.iter()
        .find(|p| p.0.len() == len)
        .unwrap()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_part1_example1() {
        let input = include_str!("example1");
        let input = parse_input(input);
        let expected = 26;

        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 301;
        let actual = solve(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_build_lookup_table() {
        use Segment::*;

        let input = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf";
        let input = parse_entry(input);
        let expected: BTreeMap<Pattern, &'static str> = vec![
            (Pattern::new(&[A, C, E, D, G, F, B]), "8"),
            (Pattern::new(&[C, D, F, B, E]), "5"),
            (Pattern::new(&[G, C, D, F, A]), "2"),
            (Pattern::new(&[F, B, C, A, D]), "3"),
            (Pattern::new(&[D, A, B]), "7"),
            (Pattern::new(&[C, E, F, A, B, D]), "9"),
            (Pattern::new(&[C, D, F, G, E, B]), "6"),
            (Pattern::new(&[E, A, F, B]), "4"),
            (Pattern::new(&[C, A, G, E, D, B]), "0"),
            (Pattern::new(&[A, B]), "1"),
        ]
            .into_iter()
            .collect();

        let actual = build_lookup_table(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_part2_example1() {
        let input = include_str!("example1");
        let input = parse_input(input);
        let expected = 61229;

        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solution2() {
        let input = include_str!("input");
        let input = parse_input(input);

        let expected = 908067;
        let actual = solve2(&input);

        assert_eq!(actual, expected)
    }
}
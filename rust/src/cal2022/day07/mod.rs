use itertools::Itertools;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{line_ending, space1};
use nom::character::streaming::not_line_ending;
use nom::combinator::map;
use nom::IResult;
use nom::multi::separated_list1;
use nom::sequence::separated_pair;

pub fn main() {
    let input = include_str!("input");

    let input = parse_input(input);

    let part1 = solve_part1(&input);

    println!("The solution to part 1 is {}", part1);

    let part2 = solve_part2(&input);
    println!("The solution to part 2 is {}", part2);
}

#[derive(Debug, Eq, PartialEq)]
struct Input {
    commands: Vec<Command>,
}

#[derive(Debug, Eq, PartialEq)]
enum Command {
    Cd(Path),
    Dir(DirContents),
}

#[derive(Debug, Eq, PartialEq)]
enum Path {
    Parent,
    Relative(String),
    Absolute(String),
}

#[derive(Debug, Eq, PartialEq)]
struct DirContents {
    entries: Vec<DirEntry>,
}

#[derive(Debug, Eq, PartialEq)]
enum DirEntry {
    Directory(String),
    File(File),
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct File {
    name: String,
    size: u32,
}

fn parse_input(s: &str) -> Input {
    let (_, commands) = separated_list1(line_ending, parse_command)(s).unwrap();

    Input { commands }
}

fn parse_command(s: &str) -> IResult<&str, Command> {
    alt((
        parse_cd,
        parse_ls,
    ))(s)
}

fn parse_cd(s: &str) -> IResult<&str, Command> {
    let (rem, _) = tag("$ cd ")(s)?;
    let (rem, path) = parse_path(rem)?;

    Ok((rem, Command::Cd(path)))
}

fn parse_path(s: &str) -> IResult<&str, Path> {
    alt((
        parse_absolute_path,
        parse_parent_path,
        parse_relative_path,
    ))(s)
}

fn parse_parent_path(s: &str) -> IResult<&str, Path> {
    map(tag(".."), |_| Path::Parent)(s)
}

fn parse_relative_path(s: &str) -> IResult<&str, Path> {
    map(not_line_ending, |s: &str| Path::Relative(s.to_owned()))(s)
}

fn parse_absolute_path(s: &str) -> IResult<&str, Path> {
    map(tag("/"), |a: &str| Path::Absolute(a.to_owned()))(s)
}

fn parse_ls(s: &str) -> IResult<&str, Command> {
    let (rem, _) = tag("$ ls")(s)?;
    let (rem, _) = line_ending(rem)?;
    let (rem, dir) = parse_dir_contents(rem)?;

    Ok((rem, Command::Dir(dir)))
}

fn parse_dir_contents(s: &str) -> IResult<&str, DirContents> {
    let (rem, entries) = separated_list1(line_ending, parse_dir_entry)(s)?;

    Ok((rem, DirContents { entries }))
}

fn parse_dir_entry(s: &str) -> IResult<&str, DirEntry> {
    alt((
        parse_dir,
        parse_file,
    ))(s)
}

fn parse_dir(s: &str) -> IResult<&str, DirEntry> {
    let (rem, _) = tag("dir")(s)?;
    let (rem, _) = space1(rem)?;
    let (rem, name) = not_line_ending(rem)?;

    let name = name.to_owned();

    Ok((rem, DirEntry::Directory(name)))
}

fn parse_file(s: &str) -> IResult<&str, DirEntry> {
    let (rem, (size, name)) = separated_pair(
        nom::character::complete::u32,
        space1,
        not_line_ending,
    )(s)?;

    let file = File {
        size,
        name: name.to_owned(),
    };

    Ok((rem, DirEntry::File(file)))
}

fn solve_part1(input: &Input) -> u32 {
    let mut fs = FileSystem::new();

    for command in &input.commands {
        match command {
            Command::Cd(path) => fs.cd(path),
            Command::Dir(dir) => {
                for entry in &dir.entries {
                    match entry {
                        DirEntry::Directory(name) => {
                            fs.mkdir(name);
                        }
                        DirEntry::File(file) => {
                            fs.make_file(file);
                        }
                    }
                }
            }
        }
    }

    let all_dirs: Vec<usize> = fs.nodes.iter()
        .enumerate()
        .filter_map(|(i, n)| match n {
            Node::Directory(_) => Some(i),
            Node::File(_) => None,
        })
        .collect();

    all_dirs.iter()
        .map(|i| fs.total_size(*i))
        .filter(|size| *size <= 100000)
        .sum()
}

#[derive(Debug)]
enum Node {
    Directory(Directory),
    File(File),
}

#[derive(Debug)]
struct Directory {
    name: String,
    children: Vec<usize>,
}

#[derive(Debug)]
struct FileSystem {
    cwd: usize,
    nodes: Vec<Node>,
}

impl FileSystem {
    fn new() -> Self {
        let root = Node::Directory(Directory {
            name: "/".into(),
            children: vec![],
        });

        let nodes = vec![root];

        Self {
            cwd: 0,
            nodes,
        }
    }

    fn make_file(&mut self, file: &File) {
        let file = Node::File(file.clone());
        self.nodes.push(file);
        let file_index = self.nodes.len() - 1;
        let node = &mut self.nodes[self.cwd];
        match node {
            Node::Directory(d) => {
                d.children.push(file_index);
            }
            Node::File(_) => unreachable!("file should not be cwd")
        }
    }

    fn mkdir(&mut self, name: &str) -> usize {
        let node = Node::Directory(Directory {
            name: name.to_owned(),
            children: vec![],
        });

        self.nodes.push(node);
        let new_dir_index = self.nodes.len() - 1;

        match &mut self.nodes[self.cwd] {
            Node::Directory(directory) => {
                directory.children.push(new_dir_index);
            }
            Node::File(_) => panic!("cwd should never be a file"),
        }

        new_dir_index
    }

    fn cd(&mut self, path: &Path) {
        match path {
            Path::Parent => {
                if let Some((i, _)) = self.nodes.iter()
                    .enumerate()
                    .find(|(_, n)| match n {
                        Node::Directory(directory) => {
                            directory.children.contains(&self.cwd)
                        }
                        Node::File(_) => false,
                    }) {
                    self.cwd = i;
                }
            }
            Path::Relative(name) => {
                let node = &self.nodes[self.cwd];

                let children = match node {
                    Node::Directory(directory) => {
                        directory.children.clone()
                    }
                    Node::File(_) => panic!("cwd should never be a file"),
                };

                for child in children {
                    let child_node = &self.nodes[child];
                    match child_node {
                        Node::Directory(directory) => {
                            if &directory.name == name {
                                // directory already exists
                                self.cwd = child;
                                return;
                            }
                        }
                        Node::File(_) => {}
                    }
                }

                // directory doesn't exist yet
                self.cwd = self.mkdir(name);
            }
            Path::Absolute(path) => {
                let name = path.clone();
                for (i, n) in self.nodes.iter().enumerate() {
                    match n {
                        Node::Directory(directory) => {
                            if directory.name == name {
                                self.cwd = i;
                                return;
                            }
                        }
                        Node::File(file) => {
                            if file.name == name {
                                panic!("trying to cd to a file");
                            }
                        }
                    }
                }

                // // dir doesn't exist

                println!("{path}");
                // do we even need to support cd to absolute dir that isn't "/"?
                // we aren't handling if the absolute path is multi levels deep

                unimplemented!()
            }
        }
    }

    fn total_size(&self, index: usize) -> u32 {
        let node = &self.nodes[index];
        match node {
            Node::Directory(d) => d.children.iter()
                .map(|i| self.total_size(*i))
                .sum(),
            Node::File(f) => f.size,
        }
    }
}

fn solve_part2(input: &Input) -> u32 {
    let mut fs = FileSystem::new();

    for command in &input.commands {
        match command {
            Command::Cd(path) => fs.cd(path),
            Command::Dir(dir) => {
                for entry in &dir.entries {
                    match entry {
                        DirEntry::Directory(name) => {
                            fs.mkdir(name);
                        }
                        DirEntry::File(file) => {
                            fs.make_file(file);
                        }
                    }
                }
            }
        }
    }

    let all_dirs: Vec<usize> = fs.nodes.iter()
        .enumerate()
        .filter_map(|(i, n)| match n {
            Node::Directory(_) => Some(i),
            Node::File(_) => None,
        })
        .collect();

    let all_dir_sizes: Vec<(usize, u32)> = all_dirs.iter()
        .map(|i| (*i, fs.total_size(*i)))
        .collect();

    let total_disk_space = 70000000;
    let required_free_space = 30000000;
    let currently_used = fs.total_size(0);

    all_dir_sizes.iter()
        .filter_map(|(i, dir_size)| {
            let free_if_deleted = total_disk_space - currently_used + dir_size;
            if free_if_deleted >= required_free_space {
                Some((i, dir_size))
            } else {
                None
            }
        })
        .sorted_by_key(|(_, dir_size)| **dir_size)
        .map(|(_, dir_size)| *dir_size)
        .next()
        .unwrap()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_solve_part1_example() {
        let input = parse_input(include_str!("example"));
        let expected = 95437;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part1() {
        let input = parse_input(include_str!("input"));
        let expected = 1084134;
        let actual = solve_part1(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2_example() {
        let input = parse_input(include_str!("example"));
        let expected = 24933642;
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }

    #[test]
    fn test_solve_part2() {
        let input = parse_input(include_str!("input"));
        let expected = 6183184;
        let actual = solve_part2(&input);

        assert_eq!(actual, expected)
    }
}
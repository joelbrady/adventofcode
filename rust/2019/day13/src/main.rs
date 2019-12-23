use input::get_input;
use intcode::{parse_program, Machine, StoppedState};
use std::io::{stdin, Read};

fn main() {
    let program = parse_program(&get_input("2019/day13/input"));
    println!("the solution to part 1 is {}", part1(&program));
    println!("the solution to part 2 is {}", part2(&program));
}

fn part1(program: &[i64]) -> usize {
    let mut m = Machine::new_feedback_mode(program);
    m.run();
    let output = m.dump_output_buffer();
    let mut sum = 0;
    for i in 0..(output.len() / 3) {
        if output[(i * 3) + 2] == 2 {
            sum += 1;
        }
    }
    sum
}

const ROWS: u32 = 23;
const COLS: u32 = 43;

fn part2(program: &[i64]) -> i64 {
    let mut m = Machine::new_feedback_mode(program);
    // insert 2 quarters
    m.set_memory_at_location(0, 2);
    let mut segment_display = [Tile::Empty; (ROWS * COLS) as usize];
    while let StoppedState::BlockedOnInput = m.run() {
        let output = m.dump_output_buffer();
        display(&output, &mut segment_display);
        let mut buf= [0u8, 1];
        stdin().lock().read_exact(&mut buf).unwrap();
        let input = buf[0] as char;
        let input = match input {
            'a' => -1,
            'd' => 1,
            _ => 0,
        };
        m.input(input);
    }
    let output = m.dump_output_buffer();
    display(&output, &mut segment_display)
}

fn display(output: &[i64], segment_display: &mut [Tile]) -> i64 {
    let (commands, score) = parse_commands(output);
    for DisplayCommand { x, y, tile } in &commands {
        let i = idx(*x, *y);
        segment_display[i] = *tile;
    }

    for y in 0..ROWS {
        for x in 0..COLS {
            let i = idx(x as i64, y as i64);
            print!("{}", segment_display[i].as_str());
        }
        println!();
    }

    println!("\nScore: {}", score);
    println!("# commands: {}", commands.len());

    score
}

fn idx(x: i64, y: i64) -> usize {
    ((y * COLS as i64) + x) as usize
}

fn parse_commands(output: &[i64]) -> (Vec<DisplayCommand>, i64) {
    let mut commands = vec![];
    let mut score: i64 = -1;
    for i in 0..(output.len() / 3) {
        let idx = i * 3;
        let x = output[idx];
        let y = output[idx + 1];
        let tile = output[idx + 2];
        if x == -1 && y == 0 {
            score = tile;
        } else {
            commands.push(DisplayCommand {
                x,
                y,
                tile: Tile::from_int(tile),
            });
        }
    }

    (commands, score)
}

struct DisplayCommand {
    x: i64,
    y: i64,
    tile: Tile,
}

#[derive(Debug, Clone, Copy)]
enum Tile {
    //0 is an empty tile. No game object appears in this tile.
    Empty,
    //1 is a wall tile. Walls are indestructible barriers.
    Wall,
    //2 is a block tile. Blocks can be broken by the ball.
    Block,
    //3 is a horizontal paddle tile. The paddle is indestructible.
    Paddle,
    //4 is a ball tile. The ball moves diagonally and bounces off objects.
    Ball,
}

impl Tile {
    fn as_str(self) -> &'static str {
        use Tile::*;

        match self {
            Empty => " ",
            Wall => "#",
            Block => "-",
            Paddle => "=",
            Ball => "o",
        }
    }

    fn from_int(value: i64) -> Tile {
        use Tile::*;

        match value {
            0 => Empty,
            1 => Wall,
            2 => Block,
            3 => Paddle,
            4 => Ball,
            _ => unimplemented!(),
        }
    }
}

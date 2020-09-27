use crate::cal2019::intcode::{Machine, parse_program, StoppedState};

pub fn main() {
    let input = include_str!("input");
    let program = parse_program(input);
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
    let mut score: i64 = 0;
    while let StoppedState::BlockedOnInput = m.run() {
        let output = m.dump_output_buffer();
        let GameState { paddle, ball, score: maybe_score } = display(&output, &mut segment_display);
//        println!("Score: {}", score);
//        sleep(Duration::new(1, 0));
        let input = if paddle.0 < ball.0 {
            1
        } else if paddle.0 == ball.0 {
            0
        } else {
            -1
        };
        if let Some(n) = maybe_score {
            score = n
        };
        m.input(input);
    }
    let output = m.dump_output_buffer();
    let gs = display(&output, &mut segment_display);
    match gs.score {
        Some(score) => score,
        _ => score,
    }
}

fn display(output: &[i64], segment_display: &mut [Tile]) -> GameState {
    let (commands, score) = parse_commands(output);
    for DisplayCommand { x, y, tile } in &commands {
        let i = idx(*x, *y);
        segment_display[i] = *tile;
    }

    let mut ball: (i64, i64) = (0, 0);
    let mut paddle: (i64, i64) = (0, 0);

    for y in 0..ROWS {
        for x in 0..COLS {
            let i = idx(x as i64, y as i64);
            let tile = segment_display[i];
//            print!("{}", tile.as_str());
            match tile {
                Tile::Paddle => paddle = (x as i64, y as i64),
                Tile::Ball => ball = (x as i64, y as i64),
                _ => continue,
            }
        }
//        println!();
    }

//    println!("# commands: {}", commands.len());
    GameState { score, paddle, ball }
}

struct GameState {
    score: Option<i64>,
    paddle: (i64, i64),
    ball: (i64, i64),
}

fn idx(x: i64, y: i64) -> usize {
    ((y * COLS as i64) + x) as usize
}

fn parse_commands(output: &[i64]) -> (Vec<DisplayCommand>, Option<i64>) {
    let mut commands = vec![];
    let mut score: Option<i64> = None;
    for i in 0..(output.len() / 3) {
        let idx = i * 3;
        let x = output[idx];
        let y = output[idx + 1];
        let tile = output[idx + 2];
        if x == -1 && y == 0 {
            score = Some(tile);
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
    #[allow(dead_code)]
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_solution() {
        let input = include_str!("input");
        let program = parse_program(input);
        assert_eq!(part1(&program), 363);
        assert_eq!(part2(&program), 17159);
    }
}

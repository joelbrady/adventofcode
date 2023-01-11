mod cal2018;
mod cal2019;
mod cal2020;
mod cal2021;
mod cal2022;
mod graph;
mod parse;

fn main() {
    call_2018();
    call_2019();
    call_2020();
    call_2021();
    call_2022();
}

fn call_2022() {
    cal2022::day01::main();
    cal2022::day02::main();
    cal2022::day03::main();
    cal2022::day04::main();
    cal2022::day05::main();
    cal2022::day06::main();
    cal2022::day07::main();
    cal2022::day08::main();
    cal2022::day09::main();
    cal2022::day10::main();
    cal2022::day11::main();
    cal2022::day12::main();

    cal2022::day21::main();
}

fn call_2021() {
    cal2021::day01::main();
    cal2021::day02::main();
    cal2021::day03::main();
    cal2021::day04::main();
    cal2021::day05::main();
    cal2021::day06::main();
    cal2021::day07::main();
    cal2021::day08::main();
    cal2021::day09::main();
    cal2021::day10::main();
}

fn call_2020() {
    cal2020::day01::main();
    cal2020::day02::main();
    cal2020::day03::main();
    cal2020::day04::main();
    cal2020::day05::main();
    cal2020::day06::main();
    cal2020::day07::main();
    cal2020::day08::main();
    cal2020::day09::main();
    cal2020::day10::main();
    cal2020::day11::main();
    cal2020::day12::main();
    cal2020::day13::main();
    cal2020::day14::main();
    cal2020::day15::main();
    cal2020::day16::main();
    cal2020::day17::main();
    cal2020::day18::main();
    cal2020::day19::main();
}

fn call_2019() {
    cal2019::day01::main();
    cal2019::day02::main();
    cal2019::day03::main();
    cal2019::day04::main();
    cal2019::day05::main();
    cal2019::day06::main();
    cal2019::day07::main();
    cal2019::day08::main();
    cal2019::day09::main();
    cal2019::day10::main();
    cal2019::day11::main();
    cal2019::day12::main();
    cal2019::day13::main();
    cal2019::day14::main();
    cal2019::day15::main();
    cal2019::day16::main();
    cal2019::day17::main();
    cal2019::day18::main();
    cal2019::day19::main();
}

fn call_2018() {
    cal2018::problem10::main().unwrap();
    cal2018::problem11::main();
    cal2018::problem13::main();
}

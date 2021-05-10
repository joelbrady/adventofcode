mod cal2018;
mod cal2019;
mod cal2020;
mod graph;
mod parse;

fn main() {
    call_2018();
    call_2019();
    call_2020();
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
    cal2018::problem10::solve().unwrap();
    cal2018::problem11::solve();
    cal2018::problem13::main();
}

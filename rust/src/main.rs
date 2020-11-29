mod cal2018;
mod cal2019;
mod graph;
mod parse;

fn main() {
    // call_2018();
    call_2019();
}

fn call_2018() {
    cal2018::problem10::solve().unwrap();
    cal2018::problem11::solve();
    cal2018::problem13::main();
}

fn call_2019() {
    // cal2019::day01::main();
    // cal2019::day02::main();
    // cal2019::day03::main();
    // cal2019::day04::main();
    // cal2019::day05::main();
    // cal2019::day06::main();
    // cal2019::day07::main();
    // cal2019::day08::main();
    // cal2019::day09::main();
    // cal2019::day10::main();
    // cal2019::day11::main();
    // cal2019::day12::main();
    // cal2019::day13::main();
    // cal2019::day14::main();
    // cal2019::day15::main();
    // cal2019::day16::main();
    // cal2019::day17::main();
    cal2019::day18::main();
}
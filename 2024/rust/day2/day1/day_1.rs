use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};

fn part1() -> io::Result<()> {
    let mut list1: Vec<i32> = Vec::new();
    let mut list2: Vec<i32> = Vec::new();

    let file = File::open("day1/input.txt")?;
    let reader = io::BufReader::new(file);
    for line in reader.lines() {
        let line = line?;
        let mut iter = line.split_whitespace();
        list1.push(iter.next().unwrap().parse::<i32>().unwrap());
        list2.push(iter.next().unwrap().parse::<i32>().unwrap());
    }

    list1.sort();
    list2.sort();

    println!(
        "Result part 1: {}",
        list1
            .iter()
            .zip(list2.iter())
            .map(|(x, y)| (x - y).abs())
            .sum::<i32>()
    );

    Ok(())
}

fn part2() -> io::Result<()> {
    let mut list1: Vec<i32> = Vec::new();
    let mut count: HashMap<i32, i32> = HashMap::new();

    let file = File::open("day1/input.txt")?;
    let reader = io::BufReader::new(file);
    for line in reader.lines() {
        let line = line?;
        let mut iter = line.split_whitespace();
        list1.push(iter.next().unwrap().parse::<i32>().unwrap());

        count
            .entry(iter.next().unwrap().parse::<i32>().unwrap())
            .and_modify(|e| *e += 1)
            .or_insert(1);
    }

    println!(
        "Result part 2: {}",
        list1
            .iter()
            .map(|x| x * count.get(x).unwrap_or(&0))
            .sum::<i32>()
    );

    Ok(())
}

fn main() -> io::Result<()> {
    part1();
    part2();
    Ok(())
}

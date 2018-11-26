use std::fs::File;
use std::io::prelude::*;

fn problem_1(content : &String) -> i32 {
	return content.lines()
		.map(|line| line.split("\t").map(|number : &str| number.parse::<i32>().unwrap()).collect::<Vec<i32>>())
		.map(|row : Vec<i32>| (row.iter().max().unwrap() - row.iter().min().unwrap()).abs())
		.sum();
}

fn problem_2(content : &String) -> i32 {
	let numbers =  content.lines()
		.map(
			|line| line.split("\t").map(|number : &str| number.parse::<i32>().unwrap()).collect::<Vec<i32>>()
		)
		.collect::<Vec<Vec<i32>>>();

	let mut result : Vec<i32> = Vec::new();
	for row in numbers {
		for number_one in &row {
			for number_two in &row {
				if number_one % number_two == 0 && number_one / number_two != 1 {
					result.push(number_one / number_two);
				}
			}
		}
	}

	return result.iter().sum();
}


fn main() {

    let filename = "input.txt";

    let mut f = File::open(filename).expect("file not found");

    let mut contents = String::new();
	f.read_to_string(&mut contents).expect("something went wrong reading the file");


	println!("Sum of max and min of rows: {}", problem_1(&contents));
	println!("Sum of division of two cleanly dividing numbers: {}", problem_2(&contents));
}

//https://stackoverflow.com/questions/11550153/determine-position-of-number-in-a-grid-of-numbers-centered-around-0-and-increasi
//https://math.stackexchange.com/questions/1263541/how-do-i-find-the-coordinate-relationship-between-numbers-on-a-number-spiral


fn first(cycle: i32) -> i32 {
    let x: i32 = (2 * cycle) - 1;
    return (x * x) + 1;
}

fn cycle(index: i32) -> i32 {
	let square_root_of_index = ((index - 1) as f64).sqrt();
    return ((square_root_of_index + 1 as f64) / 2 as f64).floor() as i32;
}

fn length(cycle: i32) -> i32 {
    return 8 * cycle
}

fn sector(index: i32) -> i32 {
	let offset = index - first(cycle(index));
	let index_length = length(cycle(index));
	return ((4 * offset / index_length) as f64).floor() as i32;
}

fn position(index: i32) -> (i32, i32) {
	if index == 1  {
		return (0, 0);
	}
	let c = cycle(index);
	let s = sector(index);
	let offset = index - first(c) - s * length(c) / 4;

	if s == 0 {
		return(-c, -c + offset + 1);
	} else if s == 1 {
		return (-c + offset + 1, c);
	} else if s == 2 {
		return (c, c - offset - 1);
	} else {
		return (c - offset - 1, -c);
	}
}

fn distance(coordinate: (i32, i32)) -> i32 {
	return coordinate.0.abs() + coordinate.1.abs();
}

fn main() {
	println!("{:?}", distance(position(312051)));
}

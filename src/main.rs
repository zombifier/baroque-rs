extern crate baroque;
use baroque::baroque::*;
use std::cmp::Ordering;
use std::io;

fn read_line() -> io::Result<Vec<usize>> {
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;
    let input_coords: Vec<usize> = input.split(' ').filter_map(|s| s.trim().parse().ok()).collect();
    if input_coords.len() != 4 {
        Err(io::Error::new(io::ErrorKind::InvalidInput, "Four valid numbers were not provided"))
    } else {
        Ok(input_coords)
    }
}

fn main() {
    let mut board = Board::new_board();
    let err = "Unknown error!";
    loop {
        board.display();
        match read_line() {
            Ok(input) => {
                match board.make_move(
                    Coord::new(input[0], input[1]), Coord::new(input[2], input[3])) {
                    Ok(messages) => {
                        for s in messages {
                            println!("{}", s);
                        }
                    },
                    Err(message) => println!("{}", message),
                }
            },
            Err(e) => eprintln!("{:?}", e),
        }
    }
}

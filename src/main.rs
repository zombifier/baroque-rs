extern crate baroque;
use baroque::baroque::*;
use baroque::players::*;

fn main() {
    let mut board = Board::new_board();
    let white = Human::new(Side::White);
    let black = AI::new(Side::Black);
    let mut current_player: &Player = &white; 
    loop {
        board.display();
        match current_player.play(&board) {
            Some((begin, end)) => match board.make_move(begin, end) {
                Ok(messages) => {
                    for s in messages {
                        println!("{}", s);
                    }
                    current_player = match current_player.get_side() {
                        Side::Black => &white,
                        Side::White => &black,
                    };
                },
                Err(message) => println!("{}", message),
            },
            None => panic!("STALEMATE! FEATURE COMING SOON"),
        }
    }
}

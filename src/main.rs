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
            Some((begin, end)) => {
                let result = board.make_move(begin, end);
                for s in result.1 {
                    println!("{}", s);
                }
                if let Some(new_board) = result.0 {
                        current_player = match current_player.get_side() {
                            Side::Black => &white,
                            Side::White => &black,
                        };
                        board = new_board;
                }
            },
            None => panic!("STALEMATE! FEATURE COMING SOON"),
        }
    }
}

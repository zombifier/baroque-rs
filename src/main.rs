extern crate baroque;
use std::rc::Rc;
use baroque::baroque::*;
use baroque::players::*;
use baroque::thread_pool::*;

fn main() {
    let mut board = Board::new_board();
    let pool = Rc::new(ThreadPool::new(12));
    let white: Box<Player> =
        match std::env::args().nth(1).and_then(|s| s.parse().ok()).unwrap_or(0) {
            n if n > 0 && n < 3 => Box::new(MinimaxThreadedAI::new(Side::White, n, &pool)),
            _ => Box::new(Human{}),
        };
    let black: Box<Player> =
        match std::env::args().nth(2).and_then(|s| s.parse().ok()).unwrap_or(0) {
            n if n > 0 && n < 3 => Box::new(MinimaxThreadedAI::new(Side::Black, n, &pool)),
            _ => Box::new(Human{}),
        };
    let mut current_player: &Player = &*white;
    loop {
        board.display();
        match current_player.play(&board) {
            Some((begin, end)) => {
                let result = board.make_move(begin, end);
                for s in result.1 {
                    println!("{}", s);
                }
                if let Some(new_board) = result.0 {
                    current_player = match board.get_current_side() {
                        Side::Black => &*white,
                        Side::White => &*black,
                    };
                    board = new_board;
                }
            },
            None => {
                if board.is_in_check(board.get_current_side()) {
                    println!("Checkmate; {} has won!", board.get_current_side().flip());
                } else {
                    println!("Stalemate!");
                }
                break;
            }
        }
    }
}

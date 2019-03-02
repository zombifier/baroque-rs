extern crate baroque;
use std::rc::Rc;
use baroque::baroque::*;
use baroque::players::*;
use baroque::thread_pool::*;

fn main() {
    let mut board = Board::new_board();
    let white = Human{};

    let pool = Rc::new(ThreadPool::new(12));
    //let white = MinimaxThreadedAI::new(Side::White, 2, &pool);
    let black = MinimaxThreadedAI::new(Side::Black, 3, &pool);
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
                    current_player = match board.get_current_side() {
                        Side::Black => &white,
                        Side::White => &black,
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

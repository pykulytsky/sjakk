use sjakk::board::{Board, Status};
use sjakk::{
    evaluation::{material, piece_placement},
    search::alpha_beta_negamax_root_async,
};
use std::time::Duration;

fn main() {
    let executor = futures::executor::ThreadPool::new().unwrap();
    let mut board = Board::default();
    println!("material: {}", material(&board));
    println!("placement: {}", piece_placement(&board));
    println!("{}", board);
    while board.status == Status::Ongoing {
        let (_, m) = alpha_beta_negamax_root_async(&board, 7, &executor, Duration::from_secs(3));
        let m = m.unwrap();
        unsafe { board.make_move_unchecked(&m) };

        print!("\x1B[2J\x1B[1;1H");
        println!("material: {}", material(&board));
        println!("placement: {}", piece_placement(&board));
        println!("{board}");
    }
}

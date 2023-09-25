use sjakk::board::{Board, Status};

fn main() {
    let executor = futures::executor::ThreadPool::new().unwrap();
    let mut board = Board::default();
    println!("evaluation: {}", board.evaluate_relative());
    println!("{}", board);
    while board.status == Status::Ongoing {
        let (_, m) = board.alpha_beta_negamax_root_async(6, &executor);
        let m = m.unwrap();
        unsafe { board.make_move_unchecked(&m) };

        print!("\x1B[2J\x1B[1;1H");
        println!("{}", board.evaluate_relative());
        println!("{board}");
    }
}

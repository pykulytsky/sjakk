use sjakk::{
    board::{Board, Status},
    Move,
};
use sjakk::{search::alpha_beta_negamax_root_async, Square};
use std::{str::FromStr, time::Duration};

fn main() {
    let executor = futures::executor::ThreadPool::new().unwrap();
    let mut board = Board::default();
    while board.status == Status::Ongoing {
        let (score, m) =
            alpha_beta_negamax_root_async(&board, 6, &executor, Duration::from_secs(10));
        let m = m.unwrap();
        unsafe { board.make_move_unchecked(&m) };

        print!("\x1B[2J\x1B[1;1H");
        println!("score: {score}");
        println!("{m}");
        println!("{board}");

        loop {
            let mut m = String::new();
            std::io::stdin().read_line(&mut m).unwrap();
            let m = m.trim();
            let m = parse_move(m);
            if board.make_move(&m).is_ok() {
                break;
            } else {
                println!("illegal move");
            }
        }
    }
}

fn parse_move(m: &str) -> Move {
    assert!(m.len() <= 5);
    let from = Square::from_str(&m[..2]).unwrap();
    let to = Square::from_str(&m[2..4]).unwrap();
    if m.len() == 4 {
        Move::new(from, to)
    } else {
        match m.chars().last() {
            Some('c') => Move::new_castle(from, to),
            Some('p') => Move::new_promotion(from, to, sjakk::PieceType::Queen),
            Some('e') => Move::new_en_passant(from, to),
            _ => unreachable!(),
        }
    }
}

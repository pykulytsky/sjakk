use sjakk::board::Board;

fn main() {
    let board = Board::from_fen("r3k2r/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQq - 0 1").unwrap();
    let moves = board.legal_moves();
    for m in moves.iter() {
        println!("{m}");
    }
}

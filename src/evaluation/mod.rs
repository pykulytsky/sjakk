use crate::{
    board::{Board, Status},
    constants::MASK_FILE,
    evaluation::params::TEMPO,
    evaluation::{params::ROOK_ON_OPEN, pawns::evaluate_pawns},
    evaluation::{params::ROOK_ON_SEMIOPEN, piece_square_tables::*},
    Color, PieceType,
};

pub mod params;
pub mod pawns;
mod piece_square_tables;

#[inline]
pub fn material(board: &Board) -> i32 {
    PieceType::ALL
        .into_iter()
        .map(|p| {
            p.value()
                * (board.pieces(Color::White)[p as usize].0.count_ones() as i32
                    - board.pieces(Color::Black)[p as usize].0.count_ones() as i32)
        })
        .reduce(|acc, m| acc + m)
        .unwrap()
}

#[inline]
pub fn mobility(board: &mut Board) -> i32 {
    let original_side = board.side_to_move;
    board.side_to_move = Color::White;
    let white_moves = board.legal_moves();
    board.side_to_move = Color::Black;
    let black_moves = board.legal_moves();
    board.side_to_move = original_side;

    white_moves.len() as i32 - black_moves.len() as i32
}

pub fn piece_placement(board: &Board) -> i32 {
    let mut white = 0;
    let mut black = 0;
    let endgame = is_endgame(board);
    if endgame {
        board.pieces(Color::White)[PieceType::King as usize]
            .into_iter()
            .for_each(|sq| white += WHITE_KING_END_GAME[sq.0 as usize]);
        board.pieces(Color::Black)[PieceType::King as usize]
            .into_iter()
            .for_each(|sq| black += BLACK_KING_END_GAME[sq.0 as usize]);
    } else {
        board.pieces(Color::White)[PieceType::King as usize]
            .into_iter()
            .for_each(|sq| white += WHITE_KING_MIDDLE_GAME[sq.0 as usize]);
        board.pieces(Color::Black)[PieceType::King as usize]
            .into_iter()
            .for_each(|sq| black += BLACK_KING_MIDDLE_GAME[sq.0 as usize]);
    }

    board.pieces(Color::White)[PieceType::Pawn as usize]
        .into_iter()
        .for_each(|sq| white += WHITE_PAWNS[sq.0 as usize]);
    board.pieces(Color::Black)[PieceType::Pawn as usize]
        .into_iter()
        .for_each(|sq| black += BLACK_PAWNS[sq.0 as usize]);

    board.pieces(Color::White)[PieceType::Knight as usize]
        .into_iter()
        .for_each(|sq| white += WHITE_KNIGHT[sq.0 as usize]);
    board.pieces(Color::Black)[PieceType::Knight as usize]
        .into_iter()
        .for_each(|sq| black += BLACK_KNIGHT[sq.0 as usize]);

    board.pieces(Color::White)[PieceType::Bishop as usize]
        .into_iter()
        .for_each(|sq| white += WHITE_BISHOP[sq.0 as usize]);
    board.pieces(Color::Black)[PieceType::Bishop as usize]
        .into_iter()
        .for_each(|sq| black += BLACK_BISHOP[sq.0 as usize]);

    board.pieces(Color::White)[PieceType::Rook as usize]
        .into_iter()
        .for_each(|sq| white += WHITE_ROOK[sq.0 as usize]);
    board.pieces(Color::Black)[PieceType::Rook as usize]
        .into_iter()
        .for_each(|sq| black += BLACK_ROOK[sq.0 as usize]);

    board.pieces(Color::White)[PieceType::Queen as usize]
        .into_iter()
        .for_each(|sq| white += WHITE_QUEEN[sq.0 as usize]);
    board.pieces(Color::Black)[PieceType::Queen as usize]
        .into_iter()
        .for_each(|sq| black += BLACK_QUEEN[sq.0 as usize]);

    if !endgame {
        let pawns = board.pieces(Color::White)[PieceType::PAWN]
            | board.pieces(Color::Black)[PieceType::PAWN];
        white += board.pieces(Color::White)[PieceType::ROOK]
            .into_iter()
            .filter(|sq| MASK_FILE[sq.file() as usize] & pawns.0 == 0)
            .count() as isize
            * ROOK_ON_OPEN as isize;
        black += board.pieces(Color::Black)[PieceType::ROOK]
            .into_iter()
            .filter(|sq| MASK_FILE[sq.file() as usize] & pawns.0 == 0)
            .count() as isize
            * ROOK_ON_OPEN as isize;
        white += board.pieces(Color::White)[PieceType::ROOK]
            .into_iter()
            .filter(|sq| {
                MASK_FILE[sq.file() as usize] & board.pieces(Color::White)[PieceType::PAWN].0 == 0
                    && MASK_FILE[sq.file() as usize] & board.pieces(Color::Black)[PieceType::PAWN].0
                        != 0
            })
            .count() as isize
            * ROOK_ON_SEMIOPEN as isize;
        black += board.pieces(Color::Black)[PieceType::ROOK]
            .into_iter()
            .filter(|sq| {
                MASK_FILE[sq.file() as usize] & board.pieces(Color::Black)[PieceType::PAWN].0 == 0
                    && MASK_FILE[sq.file() as usize] & board.pieces(Color::White)[PieceType::PAWN].0
                        != 0
            })
            .count() as isize
            * ROOK_ON_SEMIOPEN as isize;
    }
    (white - black) as i32
}

fn is_endgame(board: &Board) -> bool {
    if board.all_pieces().0.count_ones() <= 12 {
        return true;
    }
    false
}

fn sufficient_material(board: &Board) -> bool {
    let white = board.pieces(Color::White);
    let black = board.pieces(Color::Black);
    white[PieceType::QUEEN].popcnt() > 0
        || black[PieceType::QUEEN].popcnt() > 0
        || white[PieceType::ROOK].popcnt() > 0
        || black[PieceType::ROOK].popcnt() > 0
        || white[PieceType::PAWN].popcnt() > 0
        || black[PieceType::PAWN].popcnt() > 0
        || white[PieceType::BISHOP].popcnt() > 1
        || black[PieceType::BISHOP].popcnt() > 1
        || (white[PieceType::BISHOP].popcnt() > 0 && white[PieceType::KNIGHT].popcnt() > 0)
        || (black[PieceType::BISHOP].popcnt() > 0 && black[PieceType::KNIGHT].popcnt() > 0)
        || white[PieceType::KNIGHT].popcnt() > 2
        || black[PieceType::KNIGHT].popcnt() > 2
}

#[inline]
pub fn evaluate(board: &Board) -> i32 {
    if !sufficient_material(board) {
        return 0;
    }
    if let Status::Draw(_) = board.status {
        return 0;
    }
    let tempo = if is_endgame(board) {
        0
    } else {
        TEMPO * board.side_to_move.eval_mask()
    };
    material(board) + piece_placement(board) + evaluate_pawns(board) + tempo
}

#[inline]
pub fn evaluate_relative(board: &Board) -> i32 {
    evaluate(board) * (board.side_to_move.eval_mask())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::board::Board;

    #[test]
    fn piece_placement_in_standart_position() {
        let board = Board::default();
        assert_eq!(piece_placement(&board), 0);
    }
}

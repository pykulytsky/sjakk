use crate::{
    board::Board,
    constants::MASK_FILE,
    evaluation::{
        is_endgame,
        params::{PASSED_PAWN_END, PASSED_PAWN_MIDDLE},
    },
    Bitboard, Color, PieceType,
};
use crate::{
    evaluation::params::{BACKWARD_PAWN, DOUBLED_PAWN, ISOLATED_PAWN},
    piece::Pawn,
};

#[inline]
pub fn evaluate_pawns(board: &Board) -> i32 {
    let mut white = 0;
    let mut black = 0;
    let white_pawns = board.pieces(Color::White)[PieceType::PAWN];
    let black_pawns = board.pieces(Color::Black)[PieceType::PAWN];
    for (i, file) in MASK_FILE.iter().enumerate() {
        let adjacent_files = match i {
            0 => MASK_FILE[i + 1],
            1..=6 => MASK_FILE[i - 1] | MASK_FILE[i + 1],
            7 => MASK_FILE[i - 1],
            _ => unreachable!(),
        };
        if (white_pawns.0 & *file).count_ones() > 1 {
            white -= DOUBLED_PAWN * white_pawns.0.count_ones() as i32;
        }
        if (white_pawns.0 & MASK_FILE[i]) != 0 && (white_pawns.0 & adjacent_files) == 0 {
            white -= ISOLATED_PAWN;
        }

        if (black_pawns.0 & *file).count_ones() > 1 {
            black -= DOUBLED_PAWN * black_pawns.0.count_ones() as i32;
        }
        if (black_pawns.0 & MASK_FILE[i]) != 0 && (black_pawns.0 & adjacent_files) == 0 {
            black -= ISOLATED_PAWN;
        }

        let adjacent_files = match i {
            0 => MASK_FILE[i] | MASK_FILE[i + 1],
            1..=6 => MASK_FILE[i - 1] | MASK_FILE[i] | MASK_FILE[i + 1],
            7 => MASK_FILE[i] | MASK_FILE[i - 1],
            _ => unreachable!(),
        };
        let passed_pawn_price = if is_endgame(board) {
            PASSED_PAWN_END
        } else {
            PASSED_PAWN_MIDDLE
        };
        if (white_pawns.0 & MASK_FILE[i]) != 0 && (black_pawns & adjacent_files).0 == 0 {
            white += passed_pawn_price;
        }

        if (black_pawns.0 & MASK_FILE[i]) != 0 && (white_pawns & adjacent_files).0 == 0 {
            black += passed_pawn_price;
        }
    }
    let backwards_white = backward_pawns_white(white_pawns.0, black_pawns.0);
    let backwards_black = backward_pawns_black(black_pawns.0, white_pawns.0);
    white -= backwards_white.count_ones() as i32 * BACKWARD_PAWN;
    black -= backwards_black.count_ones() as i32 * BACKWARD_PAWN;
    white - black
}

fn pawn_attacks(pawns: Bitboard, color: Color) -> u64 {
    let mut res = 0;
    for p in pawns {
        res |= Pawn::pawn_attacks(color, p);
    }
    res
}

pub fn backward_pawns_white(white: u64, black: u64) -> u64 {
    let stops = white << 8;
    let white_attacks_span = pawn_attacks(Bitboard(white), Color::White);
    let black_attacks = pawn_attacks(Bitboard(black), Color::Black);
    (stops & black_attacks & !white_attacks_span) >> 8
}

pub fn backward_pawns_black(black: u64, white: u64) -> u64 {
    let stops = white >> 8;
    let black_attacks_span = pawn_attacks(Bitboard(white), Color::White);
    let white_attacks = pawn_attacks(Bitboard(black), Color::Black);
    (stops & !white_attacks & black_attacks_span) << 8
}

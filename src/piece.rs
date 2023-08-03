use crate::{
    constants::{CLEAR_FILE, MASK_RANK},
    Bitboard, Square,
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Color {
    White,
    Black,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum PieceType {
    Pawn,
    Rook,
    Knight,
    Bishop,
    Queen,
    King,
}

impl std::fmt::Display for PieceType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let notation = match self {
            PieceType::Pawn => "p",
            PieceType::Rook => "r",
            PieceType::Knight => "n",
            PieceType::Bishop => "b",
            PieceType::Queen => "q",
            PieceType::King => "k",
        };

        write!(f, "{notation}")
    }
}

pub trait Piece {
    fn pseudo_legal_moves(square: Square, color: Color, enemy: Bitboard, own: Bitboard)
        -> Bitboard;
}

pub struct Pawn;

impl Piece for Pawn {
    fn pseudo_legal_moves(
        square: Square,
        color: Color,
        enemy: Bitboard,
        own: Bitboard,
    ) -> Bitboard {
        let sq = square.as_square_number();
        let sq = Bitboard::from_square_number(sq);
        let all_pieces = enemy | own;
        let one_step = match color {
            Color::White => (sq << 8) & !all_pieces.0,
            Color::Black => (sq >> 8) & !all_pieces.0,
        };
        let two_steps = match color {
            Color::White => ((one_step & MASK_RANK[2]) << 8) & !all_pieces.0,
            Color::Black => ((one_step & MASK_RANK[5]) >> 8) & !all_pieces.0,
        };
        let valid_moves = one_step | two_steps;
        let (left_attack, right_attack) = match color {
            Color::White => ((sq & CLEAR_FILE[0]) << 7, (sq & CLEAR_FILE[7]) << 9),
            Color::Black => ((sq & CLEAR_FILE[7]) >> 7, (sq & CLEAR_FILE[0]) >> 9),
        };
        let pawn_attacks = left_attack | right_attack;
        let valid_attacks = pawn_attacks & own.0;

        let valid_moves = valid_moves | valid_attacks;

        valid_moves
    }
}

use crate::{constants, moves::Move};
use crate::{piece::Color, Bitboard};

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub enum Status {
    Checkmate,
    Stalemate,
    Ongoing,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq)]
pub struct Board {
    white_pieces: [Bitboard; 6],
    black_pieces: [Bitboard; 6],
    move_list: Vec<Move>,
    side_to_move: Color,
    status: Status,
}

impl Board {
    pub fn white(&self) -> Bitboard {
        self.white_pieces
            .into_iter()
            .reduce(|acc, next| acc | next)
            .unwrap()
    }

    pub fn black(&self) -> Bitboard {
        self.black_pieces
            .into_iter()
            .reduce(|bb, next| bb | next)
            .unwrap()
    }

    pub fn all_pieces(&self) -> Bitboard {
        self.white() | self.black()
    }
}

impl Default for Board {
    fn default() -> Self {
        Self {
            white_pieces: [
                Bitboard(constants::WHITE_PAWNS),
                Bitboard(constants::WHITE_ROOKS),
                Bitboard(constants::WHITE_KNIGHTS),
                Bitboard(constants::WHITE_BISHOPS),
                Bitboard(constants::WHITE_QUEENS),
                Bitboard(constants::WHITE_KING),
            ],
            black_pieces: [
                Bitboard(constants::BLACK_PAWNS),
                Bitboard(constants::BLACK_ROOKS),
                Bitboard(constants::BLACK_KNIGHTS),
                Bitboard(constants::BLACK_BISHOPS),
                Bitboard(constants::BLACK_QUEENS),
                Bitboard(constants::BLACK_KING),
            ],
            move_list: vec![],
            side_to_move: Color::White,
            status: Status::Ongoing,
        }
    }
}

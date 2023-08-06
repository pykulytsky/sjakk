use crate::{constants, moves::Move, piece::PieceType};
use crate::{piece::Color, Bitboard};
use strum::IntoEnumIterator;

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
    #[inline]
    pub fn white(&self) -> Bitboard {
        self.white_pieces
            .into_iter()
            .reduce(|acc, next| acc | next)
            .unwrap()
    }

    #[inline]
    pub fn black(&self) -> Bitboard {
        self.black_pieces
            .into_iter()
            .reduce(|bb, next| bb | next)
            .unwrap()
    }

    #[inline]
    pub fn all_pieces(&self) -> Bitboard {
        self.white() | self.black()
    }

    #[inline]
    pub fn free(&self) -> Bitboard {
        !self.all_pieces()
    }

    #[inline]
    pub fn pseudo_legal_moves(&self) -> Bitboard {
        match self.side_to_move {
            Color::White => self.pseudo_legal_moves_white(),
            Color::Black => self.pseudo_legal_moves_black(),
        }
    }

    #[inline]
    fn pseudo_legal_moves_white(&self) -> Bitboard {
        let mut pseudo_legal_moves = Bitboard(0);
        for piece in PieceType::iter() {
            for sq in self.white_pieces[piece as usize] {
                pseudo_legal_moves |=
                    piece.pseudo_legal_moves(sq, Color::White, self.all_pieces(), self.white());
            }
        }
        pseudo_legal_moves
    }

    #[inline]
    fn pseudo_legal_moves_black(&self) -> Bitboard {
        let mut pseudo_legal_moves = Bitboard(0);
        for piece in PieceType::iter() {
            for sq in self.black_pieces[piece as usize] {
                pseudo_legal_moves |=
                    piece.pseudo_legal_moves(sq, Color::Black, self.all_pieces(), self.black());
            }
        }
        pseudo_legal_moves
    }

    pub fn make_move(&mut self) {
        self.side_to_move = match self.side_to_move {
            Color::White => Color::Black,
            Color::Black => Color::White,
        };
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

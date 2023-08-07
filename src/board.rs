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
    pub fn new(white_pieces: [Bitboard; 6], black_pieces: [Bitboard; 6]) -> Self {
        Self {
            white_pieces,
            black_pieces,
            move_list: vec![],
            side_to_move: Color::White,
            status: Status::Ongoing,
        }
    }

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
    pub fn pseudo_legal_moves(&self) -> Vec<Move> {
        match self.side_to_move {
            Color::White => self.pseudo_legal_moves_white(),
            Color::Black => self.pseudo_legal_moves_black(),
        }
    }

    // #[inline]
    // fn pseudo_legal_moves_white(&self) -> Bitboard {
    //     let mut pseudo_legal_moves = Bitboard(0);
    //     for piece in PieceType::iter() {
    //         for sq in self.white_pieces[piece as usize] {
    //             pseudo_legal_moves |=
    //                 piece.pseudo_legal_moves(sq, Color::White, self.all_pieces(), self.white());
    //         }
    //     }
    //     pseudo_legal_moves
    // }
    //
    #[inline]
    pub fn pseudo_legal_moves_white(&self) -> Vec<Move> {
        let mut moves = vec![];
        for piece in PieceType::iter() {
            for sq in self.white_pieces[piece as usize] {
                for m in piece.pseudo_legal_moves(sq, Color::White, self.all_pieces(), self.white())
                {
                    moves.push(Move::new(sq, m, false, None));
                }
            }
        }
        moves
    }

    #[inline]
    pub fn pseudo_legal_moves_black(&self) -> Vec<Move> {
        let mut moves = vec![];
        for piece in PieceType::iter() {
            for sq in self.black_pieces[piece as usize] {
                for m in piece.pseudo_legal_moves(sq, Color::Black, self.all_pieces(), self.black())
                {
                    moves.push(Move::new(sq, m, false, None));
                }
            }
        }
        moves
    }

    // #[inline]
    // fn pseudo_legal_moves_black(&self) -> Bitboard {
    //     let mut pseudo_legal_moves = Bitboard(0);
    //     for piece in PieceType::iter() {
    //         for sq in self.black_pieces[piece as usize] {
    //             pseudo_legal_moves |=
    //                 piece.pseudo_legal_moves(sq, Color::Black, self.all_pieces(), self.black());
    //         }
    //     }
    //     pseudo_legal_moves
    // }
    //
    pub fn make_move(&mut self) {
        self.side_to_move = match self.side_to_move {
            Color::White => Color::Black,
            Color::Black => Color::White,
        };
    }

    #[inline]
    pub fn attacks_to_king(&self) -> Bitboard {
        let mut attacks_to_king_bitboard = Bitboard(0);
        let (king_square, oposite_side, color) = match self.side_to_move {
            Color::White => (
                self.white_pieces[PieceType::King as usize],
                self.black_pieces,
                Color::White,
            ),
            Color::Black => (
                self.black_pieces[PieceType::King as usize],
                self.white_pieces,
                Color::Black,
            ),
        };
        // Skip the [`PieceType::King`], since you can not check with king.
        for piece in PieceType::iter().take(5) {
            for sq in oposite_side[piece as usize] {
                attacks_to_king_bitboard |= king_square
                    & piece.pseudo_legal_moves(sq, color, self.all_pieces(), self.black());
            }
        }

        attacks_to_king_bitboard
    }

    #[inline]
    pub fn king_in_check(&self) -> bool {
        let king = match self.side_to_move {
            Color::White => self.white_pieces[PieceType::King as usize],
            Color::Black => self.black_pieces[PieceType::King as usize],
        };

        (king.0 & self.attacks_to_king().0) > 1
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

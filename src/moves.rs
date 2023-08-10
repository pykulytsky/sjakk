use std::fmt::Display;

use crate::{piece::PieceType, Bitboard, Color, Square};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Eq)]
pub struct Move {
    pub from: Square,
    pub to: Square,
    pub piece: PieceType,
    pub capture: Option<PieceType>,
    pub move_type: MoveType,
}

impl Move {
    pub fn new(
        from: Square,
        to: Square,
        piece: PieceType,
        capture: Option<PieceType>,
        move_type: MoveType,
    ) -> Self {
        Self {
            from,
            to,
            piece,
            capture,
            move_type,
        }
    }
    pub fn update_position(
        &self,
        white_pieces: &mut [Bitboard; 6],
        black_pieces: &mut [Bitboard; 6],
        side_to_move: Color,
    ) {
        let from_bb = Bitboard(1_u64 << self.from.0);
        let to_bb = Bitboard(1_u64 << self.to.0);
        let from_to_bb = from_bb ^ to_bb;

        match self.move_type {
            MoveType::Quiet => {
                match side_to_move {
                    Color::White => white_pieces[self.piece as usize] ^= from_to_bb,
                    Color::Black => black_pieces[self.piece as usize] ^= from_to_bb,
                }

                if let Some(piece) = self.capture {
                    match side_to_move.opposite() {
                        Color::White => white_pieces[piece as usize] ^= to_bb,
                        Color::Black => black_pieces[piece as usize] ^= to_bb,
                    }
                }
            }
            MoveType::Promotion { promotion_to } => {
                match side_to_move {
                    Color::White => {
                        white_pieces[self.piece as usize] ^= from_bb;
                        white_pieces[promotion_to as usize] ^= to_bb;
                    }
                    Color::Black => {
                        black_pieces[self.piece as usize] ^= from_bb;
                        black_pieces[promotion_to as usize] ^= to_bb;
                    }
                }

                if let Some(piece) = self.capture {
                    match side_to_move.opposite() {
                        Color::White => white_pieces[piece as usize] ^= to_bb,
                        Color::Black => black_pieces[piece as usize] ^= to_bb,
                    }
                }
            }
            MoveType::EnPassant { captures_on } => {
                match side_to_move {
                    Color::White => white_pieces[self.piece as usize] ^= from_to_bb,
                    Color::Black => black_pieces[self.piece as usize] ^= from_to_bb,
                }

                match side_to_move.opposite() {
                    Color::White => {
                        white_pieces[self.piece as usize] ^= Bitboard::from_square(captures_on)
                    }
                    Color::Black => {
                        black_pieces[self.piece as usize] ^= Bitboard::from_square(captures_on)
                    }
                }
            }
            MoveType::Castling => todo!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Eq)]
pub enum MoveType {
    Quiet,
    Promotion { promotion_to: PieceType },
    EnPassant { captures_on: Square },
    Castling,
}

impl Display for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let piece = if self.piece == PieceType::Pawn {
            "".to_string()
        } else {
            format!("{}", self.piece)
        };
        let capture = if self.capture.is_some() {
            if self.piece == PieceType::Pawn {
                format!("{}x", self.from.file())
            } else {
                "x".to_string()
            }
        } else {
            "".to_string()
        };
        let promotion = if let MoveType::Promotion { promotion_to } = self.move_type {
            format!("={}", promotion_to)
        } else {
            "".to_string()
        };

        write!(f, "{}{}{}{}", piece, capture, self.to, promotion)
    }
}

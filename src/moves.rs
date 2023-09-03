use std::{fmt::Display, ops::Index};

use crate::{piece::PieceType, Bitboard, Color, Square};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd)]
pub struct Move {
    pub from: Square,
    pub to: Square,
    pub piece: PieceType,
    pub capture: Option<PieceType>,
    pub move_type: MoveType,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Eq)]
pub enum CastlingSide {
    KingSide,
    QueenSide,
    Both,
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
        white: &mut Bitboard,
        black: &mut Bitboard,
        side_to_move: Color,
    ) {
        let from_bb = Bitboard(1_u64 << self.from.0);
        let to_bb = Bitboard(1_u64 << self.to.0);
        let from_to_bb = from_bb ^ to_bb;

        match self.move_type {
            MoveType::Quiet => {
                match side_to_move {
                    Color::White => {
                        white_pieces[self.piece as usize] ^= from_to_bb;
                        *white ^= from_to_bb;
                    }
                    Color::Black => {
                        black_pieces[self.piece as usize] ^= from_to_bb;
                        *black ^= from_to_bb;
                    }
                }

                if let Some(piece) = self.capture {
                    match side_to_move.opposite() {
                        Color::White => {
                            white_pieces[piece as usize] ^= to_bb;
                            *white ^= to_bb;
                        }
                        Color::Black => {
                            black_pieces[piece as usize] ^= to_bb;
                            *black ^= to_bb;
                        }
                    }
                }
            }
            MoveType::Promotion { promotion_to } => {
                assert!(promotion_to != PieceType::King);
                match side_to_move {
                    Color::White => {
                        white_pieces[self.piece as usize] ^= from_bb;
                        white_pieces[promotion_to as usize] ^= to_bb;
                        *white ^= from_to_bb;
                    }
                    Color::Black => {
                        black_pieces[self.piece as usize] ^= from_bb;
                        black_pieces[promotion_to as usize] ^= to_bb;
                        *black ^= from_to_bb;
                    }
                }

                if let Some(piece) = self.capture {
                    match side_to_move.opposite() {
                        Color::White => {
                            white_pieces[piece as usize] ^= to_bb;
                            *white ^= to_bb;
                        }
                        Color::Black => {
                            black_pieces[piece as usize] ^= to_bb;
                            *black ^= to_bb;
                        }
                    }
                }
            }
            MoveType::EnPassant { captures_on } => {
                match side_to_move {
                    Color::White => {
                        white_pieces[self.piece as usize] ^= from_to_bb;
                        *white ^= from_to_bb;
                    }
                    Color::Black => {
                        black_pieces[self.piece as usize] ^= from_to_bb;
                        *black ^= from_to_bb;
                    }
                }

                match side_to_move.opposite() {
                    Color::White => {
                        white_pieces[self.piece as usize] ^= Bitboard::from_square(captures_on);
                        *white ^= Bitboard::from_square(captures_on);
                    }
                    Color::Black => {
                        black_pieces[self.piece as usize] ^= Bitboard::from_square(captures_on);
                        *black ^= Bitboard::from_square(captures_on);
                    }
                }
            }
            MoveType::Castling { side } => match side_to_move {
                Color::White => {
                    white_pieces[self.piece as usize] ^= from_to_bb;
                    *white ^= from_to_bb;
                    match side {
                        CastlingSide::KingSide => {
                            let rook = Bitboard::from_square(Square::H1)
                                ^ Bitboard::from_square(Square::F1);
                            white_pieces[PieceType::Rook as usize] ^= rook;
                            *white ^= rook;
                        }
                        CastlingSide::QueenSide => {
                            let rook = Bitboard::from_square(Square::A1)
                                ^ Bitboard::from_square(Square::D1);
                            white_pieces[PieceType::Rook as usize] ^= rook;
                            *white ^= rook;
                        }
                        CastlingSide::Both => unreachable!(),
                    }
                }
                Color::Black => {
                    black_pieces[self.piece as usize] ^= from_to_bb;
                    *black ^= from_to_bb;
                    match side {
                        CastlingSide::KingSide => {
                            let rook = Bitboard::from_square(Square::H8)
                                ^ Bitboard::from_square(Square::F8);
                            black_pieces[PieceType::Rook as usize] ^= rook;
                            *black ^= rook;
                        }
                        CastlingSide::QueenSide => {
                            let rook = Bitboard::from_square(Square::A8)
                                ^ Bitboard::from_square(Square::D8);
                            black_pieces[PieceType::Rook as usize] ^= rook;
                            *black ^= rook;
                        }
                        CastlingSide::Both => unreachable!(),
                    }
                }
            },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Eq)]
pub enum MoveType {
    Quiet,
    EnPassant { captures_on: Square },
    Castling { side: CastlingSide },
    Promotion { promotion_to: PieceType },
}

impl Display for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let MoveType::Castling { side } = self.move_type {
            match side {
                CastlingSide::KingSide => {
                    write!(f, "O-O")?;
                }
                CastlingSide::QueenSide => {
                    write!(f, "O-O-O")?;
                }
                CastlingSide::Both => unreachable!(),
            }
        } else {
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

            write!(f, "{}{}{}{}", piece, capture, self.to, promotion)?;
        }

        Ok(())
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Move1 {
    repr: u16,
}

impl Move1 {
    pub const EMPTY: Self = Self { repr: 0 };
    pub fn new(from: Square, to: Square) -> Self {
        Self {
            repr: u16::from(from.0) | (u16::from(to.0) << 6),
        }
    }
    pub fn new_promotion(from: Square, to: Square, promotion: PieceType) -> Self {
        let promotion = u16::from(promotion as u8).wrapping_sub(1) & 0b11;

        Self {
            repr: u16::from(from.0)
                | (u16::from(to.0) << 6)
                | (promotion << 12)
                | 0b1100_0000_0000_0000,
        }
    }

    pub fn new_en_passant(from: Square, to: Square) -> Self {
        Self {
            repr: u16::from(from.0) | (u16::from(to.0) << 6) | 0b0100_0000_0000_0000,
        }
    }

    pub fn new_castle(from: Square, to: Square) -> Self {
        Self {
            repr: u16::from(from.0) | (u16::from(to.0) << 6) | 0b1000_0000_0000_0000,
        }
    }

    pub fn from(&self) -> Square {
        Square((self.repr & 0b11_1111) as u8)
    }

    pub fn to(&self) -> Square {
        Square(((self.repr >> 6) & 0b11_1111) as u8)
    }

    pub const fn is_promotion(&self) -> bool {
        (self.repr & 0b1100_0000_0000_0000) == 0b1100_0000_0000_0000
    }

    pub const fn is_en_passant(&self) -> bool {
        (self.repr & 0b0100_0000_0000_0000) != 0 && self.repr & 0b1000_0000_0000_0000 == 0
    }

    pub const fn is_castle(&self) -> bool {
        (self.repr & 0b1000_0000_0000_0000) != 0 && self.repr & 0b0100_0000_0000_0000 == 0
    }
    pub fn promotion_to(self) -> Option<PieceType> {
        if !self.is_promotion() {
            return None;
        }
        let output = PieceType::from_index((((self.repr >> 12) & 0b11) as u8 + 1) as usize);
        Some(output)
    }

    pub fn en_passant_target(&self) -> Option<Square> {
        if self.is_en_passant() {
            if self.from() > self.to() {
                // black pawn
                return Some(Square(self.to().0 - 8));
            } else {
                // white pawnt
                return Some(Square(self.to().0 + 8));
            }
        }
        None
    }
}

impl Display for Move1 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.from(), self.to())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn quiet_move() {
        let m = Move1::new(Square(10), Square(18));
        assert_eq!(m.from(), Square(10));
        assert_eq!(m.to(), Square(18));
    }

    #[test]
    fn castle() {
        let m = Move1::new_castle(Square(10), Square(18));
        assert!(m.is_castle());
    }

    #[test]
    fn promotion() {
        let m = Move1::new_promotion(Square(10), Square(18), PieceType::Queen);
        assert!(m.is_promotion());
        assert_eq!(m.promotion_to(), Some(PieceType::Queen));
    }

    #[test]
    fn en_passant() {
        let m = Move1::new_en_passant(Square(33), Square(40));
        assert!(m.is_en_passant());
    }
}

pub const MAX_MOVELIST_LEN: usize = 108;

#[derive(Debug, Clone)]
pub struct MoveList {
    pub inner: [Move1; MAX_MOVELIST_LEN],
    len: usize,
}

impl MoveList {
    pub const fn new() -> Self {
        Self {
            inner: [Move1::EMPTY; MAX_MOVELIST_LEN],
            len: 0,
        }
    }
    pub fn push(&mut self, m: Move1) {
        self.inner[self.len] = m;
        self.len += 1;
    }
    pub fn iter(&self) -> impl Iterator<Item = &Move1> {
        self.inner[..self.len].iter()
    }

    pub fn as_slice(&self) -> &[Move1] {
        &self.inner[..self.len]
    }

    pub fn as_slice_mut(&mut self) -> &mut [Move1] {
        &mut self.inner[..self.len]
    }
    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }
}

impl Index<usize> for MoveList {
    type Output = Move1;

    fn index(&self, index: usize) -> &Self::Output {
        &self.inner[..self.len][index]
    }
}

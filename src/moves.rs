use std::{fmt::Display, ops::Index};

use crate::{piece::PieceType, Square};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Eq)]
pub enum CastlingSide {
    KingSide,
    QueenSide,
    Both,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Move {
    repr: u16,
}

impl Move {
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
                return Some(Square(self.to().0 + 8));
            } else {
                // white pawn
                return Some(Square(self.to().0 - 8));
            }
        }
        None
    }
}

impl Display for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.from(), self.to())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn quiet_move() {
        let m = Move::new(Square(10), Square(18));
        assert_eq!(m.from(), Square(10));
        assert_eq!(m.to(), Square(18));
    }

    #[test]
    fn castle() {
        let m = Move::new_castle(Square(10), Square(18));
        assert!(m.is_castle());
    }

    #[test]
    fn promotion() {
        let m = Move::new_promotion(Square(10), Square(18), PieceType::Queen);
        assert!(m.is_promotion());
        assert_eq!(m.promotion_to(), Some(PieceType::Queen));
    }

    #[test]
    fn en_passant() {
        let m = Move::new_en_passant(Square(33), Square(40));
        assert!(m.is_en_passant());
    }
}

pub const MAX_MOVELIST_LEN: usize = 108;

#[derive(Debug, Clone)]
pub struct MoveList {
    pub inner: [Move; MAX_MOVELIST_LEN],
    len: usize,
}

impl MoveList {
    pub const fn new() -> Self {
        Self {
            inner: [Move::EMPTY; MAX_MOVELIST_LEN],
            len: 0,
        }
    }
    pub fn push(&mut self, m: Move) {
        self.inner[self.len] = m;
        self.len += 1;
    }
    pub fn iter(&self) -> impl Iterator<Item = &Move> {
        self.inner[..self.len].iter()
    }

    pub fn as_slice(&self) -> &[Move] {
        &self.inner[..self.len]
    }

    pub fn as_slice_mut(&mut self) -> &mut [Move] {
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
    type Output = Move;

    fn index(&self, index: usize) -> &Self::Output {
        &self.inner[..self.len][index]
    }
}

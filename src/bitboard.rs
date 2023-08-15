//!Implements `Bitboard` - chess position representation. Basically it is just a wrapper around
//!`u64`, where each bit corresponds to chess square.

use std::fmt::Display;

use crate::{constants::Direction, File, Rank, Square};

/// Represents particular position on a chess board.
/// It is used in variety of places, but the main purpose of bitboards is to represent position
/// for pieces of each color.
/// This struct implements all bitwise operations, so you can use it in operations with `u64`
/// numbers.
/// Example how to get all pawns in the position:
/// ```no_run
/// let white_pawns = sjakk::bitboard::Bitboard(0xFF_u64 << 8);
/// let black_pawns = white_pawns << 40;
/// let all_pawns = white_pawns | black_pawns;
/// assert_eq!(all_pawns, 0b0000000011111111000000000000000000000000000000001111111100000000);
/// ```
///
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct Bitboard(pub u64);

impl std::ops::BitOr for Bitboard {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl std::ops::BitOr<&Bitboard> for &Bitboard {
    type Output = Bitboard;
    fn bitor(self, rhs: &Bitboard) -> Bitboard {
        Bitboard(self.0 | rhs.0)
    }
}

impl std::ops::BitOr<u64> for Bitboard {
    type Output = Self;
    fn bitor(self, rhs: u64) -> Self::Output {
        Self(self.0 | rhs)
    }
}

impl std::ops::BitAnd for Bitboard {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}

impl std::ops::BitAnd for &Bitboard {
    type Output = Bitboard;
    fn bitand(self, rhs: &Bitboard) -> Bitboard {
        Bitboard(self.0 & rhs.0)
    }
}

impl std::ops::BitAnd<u64> for Bitboard {
    type Output = Self;
    fn bitand(self, rhs: u64) -> Self::Output {
        Self(self.0 & rhs)
    }
}

impl std::ops::BitXor for Bitboard {
    type Output = Self;
    fn bitxor(self, rhs: Self) -> Self::Output {
        Self(self.0 ^ rhs.0)
    }
}

impl std::ops::BitXor for &Bitboard {
    type Output = Bitboard;
    fn bitxor(self, rhs: &Bitboard) -> Bitboard {
        Bitboard(self.0 ^ rhs.0)
    }
}

impl std::ops::BitXor<u64> for Bitboard {
    type Output = Self;
    fn bitxor(self, rhs: u64) -> Self::Output {
        Self(self.0 ^ rhs)
    }
}

impl std::ops::BitOrAssign for Bitboard {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0
    }
}

impl std::ops::BitOrAssign<u64> for Bitboard {
    fn bitor_assign(&mut self, rhs: u64) {
        self.0 |= rhs
    }
}

impl std::ops::BitOrAssign<&Bitboard> for Bitboard {
    fn bitor_assign(&mut self, rhs: &Bitboard) {
        self.0 |= rhs.0
    }
}

impl std::ops::BitAndAssign for Bitboard {
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 &= rhs.0
    }
}

impl std::ops::BitAndAssign<u64> for Bitboard {
    fn bitand_assign(&mut self, rhs: u64) {
        self.0 &= rhs
    }
}

impl std::ops::BitXorAssign for Bitboard {
    fn bitxor_assign(&mut self, rhs: Self) {
        self.0 ^= rhs.0
    }
}

impl std::ops::BitXorAssign<u64> for Bitboard {
    fn bitxor_assign(&mut self, rhs: u64) {
        self.0 ^= rhs
    }
}

impl std::ops::Not for Bitboard {
    type Output = Self;
    fn not(self) -> Self::Output {
        Self(!self.0)
    }
}

impl std::ops::Shl for Bitboard {
    type Output = Self;
    fn shl(self, rhs: Self) -> Self::Output {
        Self(self.0 << rhs.0)
    }
}

impl std::ops::Shl<u64> for Bitboard {
    type Output = Self;
    fn shl(self, rhs: u64) -> Self::Output {
        Self(self.0 << rhs)
    }
}

impl std::ops::Shr for Bitboard {
    type Output = Self;
    fn shr(self, rhs: Self) -> Self::Output {
        Self(self.0 >> rhs.0)
    }
}

impl std::ops::Shr<u64> for Bitboard {
    type Output = Self;
    fn shr(self, rhs: u64) -> Self::Output {
        Self(self.0 >> rhs)
    }
}
impl std::ops::ShlAssign for Bitboard {
    fn shl_assign(&mut self, rhs: Self) {
        self.0 <<= rhs.0
    }
}

impl std::ops::ShlAssign<u64> for Bitboard {
    fn shl_assign(&mut self, rhs: u64) {
        self.0 <<= rhs
    }
}

impl std::ops::ShrAssign for Bitboard {
    fn shr_assign(&mut self, rhs: Self) {
        self.0 >>= rhs.0
    }
}

impl std::ops::ShrAssign<u64> for Bitboard {
    fn shr_assign(&mut self, rhs: u64) {
        self.0 >>= rhs
    }
}

impl PartialEq<u64> for Bitboard {
    fn eq(&self, other: &u64) -> bool {
        self.0.eq(other)
    }
}

impl From<u64> for Bitboard {
    fn from(value: u64) -> Self {
        Self(value)
    }
}

impl std::ops::BitOr<Bitboard> for u64 {
    type Output = u64;
    fn bitor(self, rhs: Bitboard) -> Self::Output {
        self | rhs.0
    }
}

impl std::ops::BitAnd<Bitboard> for u64 {
    type Output = u64;
    fn bitand(self, rhs: Bitboard) -> Self::Output {
        self & rhs.0
    }
}

impl std::ops::BitXor<Bitboard> for u64 {
    type Output = u64;
    fn bitxor(self, rhs: Bitboard) -> Self::Output {
        self ^ rhs.0
    }
}

impl std::ops::BitOrAssign<Bitboard> for u64 {
    fn bitor_assign(&mut self, rhs: Bitboard) {
        *self |= rhs.0;
    }
}

impl std::ops::BitAndAssign<Bitboard> for u64 {
    fn bitand_assign(&mut self, rhs: Bitboard) {
        *self &= rhs.0;
    }
}

impl std::ops::BitXorAssign<Bitboard> for u64 {
    fn bitxor_assign(&mut self, rhs: Bitboard) {
        *self ^= rhs.0;
    }
}

impl Display for Bitboard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f)?;
        for row in (0..8).rev() {
            write!(f, "{}", row + 1)?;
            for i in 0..8 {
                write!(f, " {} ", self.0 >> (8 * row + i) & 1)?;
            }

            writeln!(f)?;
        }
        write!(f, "  A  B  C  D  E  F  G  H ")?;
        writeln!(f)?;
        Ok(())
    }
}

impl Bitboard {
    /// Creates bitboard with `1` on the given square, with rest of the board filled with `0`.
    ///
    /// ```
    /// use sjakk::Bitboard;
    /// let bb = Bitboard::from_square_number(10);
    /// assert_eq!(bb.0.trailing_zeros(), 10);
    /// assert_eq!(bb.0.leading_zeros(), 53);
    /// ```
    pub fn from_square_number(square: u8) -> Self {
        Self(1_u64 << square)
    }

    pub fn from_square(sq: Square) -> Bitboard {
        Bitboard(1u64 << sq.0)
    }

    /// Creates empty board (filled with zeros).
    /// ```
    /// use sjakk::Bitboard;
    /// let bb = Bitboard::empty();
    /// assert_eq!(bb, 0);
    /// ```
    pub fn empty() -> Self {
        Self(0)
    }

    /// Creates board, filled with ones.
    ///
    /// ```
    /// use sjakk::Bitboard;
    /// let bb = Bitboard::universe();
    /// assert_eq!(bb, u64::MAX);
    /// ```
    pub fn universe() -> Self {
        Self(u64::MAX)
    }

    /// Move bitboard by 1 bit in [`Direction`].
    /// Use only with single bitsets.
    #[inline]
    pub fn one_step_by_direction(&self, direction: Direction) -> Bitboard {
        assert!(self.0.count_ones() == 1); // is single bitset
        let mask_a_file = 0xfefefefefefefefe_u64;
        let mask_h_file = 0x7f7f7f7f7f7f7f7f_u64;

        match direction {
            Direction::North => Bitboard(self.0 << 8),
            Direction::NorthEast => Bitboard((self.0 << 9) & mask_a_file),
            Direction::East => Bitboard((self.0 << 1) & mask_a_file),
            Direction::SouthEast => Bitboard((self.0 >> 7) & mask_a_file),
            Direction::South => Bitboard(self.0 >> 8),
            Direction::SouthWest => Bitboard((self.0 >> 9) & mask_h_file),
            Direction::West => Bitboard((self.0 >> 1) & mask_h_file),
            Direction::NorthWest => Bitboard((self.0 << 7) & mask_h_file),
        }
    }

    /// Returns true if this [`Bitboard`] is not filled with zeros.
    pub fn is_set(&self) -> bool {
        self.0 != 0
    }

    /// Returns the rank of this [`Bitboard`].
    pub fn rank(&self) -> Rank {
        assert!(self.0.count_ones() == 1); // is single bitset
        todo!()
    }

    /// Returns the file of this [`Bitboard`].
    pub fn file(&self) -> File {
        assert!(self.0.count_ones() == 1); // is single bitset
        todo!()
    }

    /// Returns [`Square`], which is represented by least significant bit.
    /// Can be used to iterate over all pieces, which exists on particular [`Bitboard`]
    pub fn lsb_square(&self) -> Square {
        Square(self.0.trailing_zeros() as u8)
    }
}

impl From<Square> for Bitboard {
    fn from(value: Square) -> Self {
        Self::from_square(value)
    }
}

impl Iterator for Bitboard {
    type Item = Square;

    #[inline]
    fn next(&mut self) -> Option<Square> {
        if self.0 == 0 {
            None
        } else {
            let lsb = self.lsb_square();
            *self ^= Bitboard::from_square(lsb);
            Some(lsb)
        }
    }
}

impl std::fmt::Debug for Bitboard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn one_ste_by_direction() {
        let bb = Bitboard::from_square(Square(10));
        let bb = bb.one_step_by_direction(Direction::East);
        assert_eq!(bb.lsb_square(), Square(11));
        let bb = bb.one_step_by_direction(Direction::West);
        assert_eq!(bb.lsb_square(), Square(10));
        let bb = bb.one_step_by_direction(Direction::North);
        assert_eq!(bb.lsb_square(), Square(18));
        let bb = bb.one_step_by_direction(Direction::South);
        assert_eq!(bb.lsb_square(), Square(10));
        let bb = bb.one_step_by_direction(Direction::NorthEast);
        assert_eq!(bb.lsb_square(), Square(19));
        let bb = bb.one_step_by_direction(Direction::SouthWest);
        assert_eq!(bb.lsb_square(), Square(10));
        let bb = bb.one_step_by_direction(Direction::NorthWest);
        assert_eq!(bb.lsb_square(), Square(17));
        let bb = bb.one_step_by_direction(Direction::SouthEast);
        assert_eq!(bb.lsb_square(), Square(10));
    }

    #[test]
    fn iterate_over_squares() {
        let bb = Bitboard(0xFF);
        assert_eq!(bb.into_iter().count(), 8);
    }
}

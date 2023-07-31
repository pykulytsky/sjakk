//!Implements `Bitboard` - chess position representation. Basically it is just a wrapper around
//!`u64`, where each bit corresponds to chess square.

use std::fmt::Display;

/// Represents particular position on a chess board.
/// It is used in variety of places, but the main purpose of bitboards is to represent position
/// for
/// pieces of each color.
/// Example how to get all pawns in the position:
/// ```no_run
/// let white_pawns = sjakk::bitboard::Bitboard(0xFF_u64 << 8);
/// let black_pawns = white_pawns << 40.into();
/// let all_pawns = white_pawns | black_pawns;
/// ```
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Bitboard(pub u64);

impl std::ops::BitOr for Bitboard {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl std::ops::BitAnd for Bitboard {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}
impl std::ops::BitXor for Bitboard {
    type Output = Self;
    fn bitxor(self, rhs: Self) -> Self::Output {
        Self(self.0 ^ rhs.0)
    }
}

impl std::ops::BitOrAssign for Bitboard {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0
    }
}

impl std::ops::BitAndAssign for Bitboard {
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 &= rhs.0
    }
}

impl std::ops::BitXorAssign for Bitboard {
    fn bitxor_assign(&mut self, rhs: Self) {
        self.0 ^= rhs.0
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

impl std::ops::Shr for Bitboard {
    type Output = Self;
    fn shr(self, rhs: Self) -> Self::Output {
        Self(self.0 >> rhs.0)
    }
}

impl std::ops::ShlAssign for Bitboard {
    fn shl_assign(&mut self, rhs: Self) {
        self.0 <<= rhs.0
    }
}

impl std::ops::ShrAssign for Bitboard {
    fn shr_assign(&mut self, rhs: Self) {
        self.0 >>= rhs.0
    }
}

impl Display for Bitboard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for row in (0..8).rev() {
            write!(f, "{}", row + 1)?;
            for i in 0..8 {
                write!(f, " {} ", self.0 >> (8 * row + i) & 1)?;
            }

            write!(f, "\n")?;
        }
        write!(f, "  A  B  C  D  E  F  G  H ")?;
        write!(f, "\n")?;
        Ok(())
    }
}

impl From<u64> for Bitboard {
    fn from(value: u64) -> Self {
        Self(value)
    }
}

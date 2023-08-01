//! Provides all nesecery representations off square and other entities, that are involved.

use strum_macros::EnumIter;

use crate::Bitboard;

#[derive(Debug, EnumIter, Clone, Copy, PartialEq, PartialOrd, Ord, Eq)]
pub enum Rank {
    Rank1,
    Rank2,
    Rank3,
    Rank4,
    Rank5,
    Rank6,
    Rank7,
    Rank8,
}

#[derive(Debug, EnumIter, Clone, Copy, PartialEq, PartialOrd, Ord, Eq)]
pub enum File {
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Square {
    rank: Rank,
    file: File,
}

impl Square {
    pub const fn new(rank: Rank, file: File) -> Self {
        Self { rank, file }
    }

    pub fn as_square_number(&self) -> u8 {
        self.rank as u8 * 8 + self.file as u8
    }
}

impl Into<Bitboard> for Square {
    fn into(self) -> Bitboard {
        Bitboard::from_square_number(self.as_square_number())
    }
}

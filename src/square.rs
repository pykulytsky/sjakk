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
pub struct Square(pub u8);
impl Square {
    pub fn from_square_and_rank(file: File, rank: Rank) -> Self {
        todo!()
    }

    pub fn file() -> File {
        todo!()
    }

    pub fn rank() -> Rank {
        todo!()
    }
}

impl Into<Bitboard> for Square {
    fn into(self) -> Bitboard {
        Bitboard::from_square_number(self.0)
    }
}

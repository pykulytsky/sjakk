//! Provides all nesecery representations of a square and other entities, that are involved.

use std::mem::transmute;

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

impl std::fmt::Display for Rank {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let number = match &self {
            Rank::Rank1 => "1",
            Rank::Rank2 => "2",
            Rank::Rank3 => "3",
            Rank::Rank4 => "4",
            Rank::Rank5 => "5",
            Rank::Rank6 => "6",
            Rank::Rank7 => "7",
            Rank::Rank8 => "8",
        };

        write!(f, "{number}")
    }
}

impl Rank {
    #[inline]
    pub fn from_index(i: usize) -> Rank {
        unsafe { transmute((i as u8) & 7) }
    }
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

impl std::fmt::Display for File {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let letter = match &self {
            File::A => "a",
            File::B => "b",
            File::C => "c",
            File::D => "d",
            File::E => "e",
            File::F => "f",
            File::G => "g",
            File::H => "h",
        };

        write!(f, "{letter}")
    }
}

impl File {
    #[inline]
    pub fn from_index(i: usize) -> File {
        unsafe { transmute((i as u8) & 7) }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Square(pub u8);
impl Square {
    /// Creates new [`Square`] from intersection of [`Rank`] and [`File`].
    ///
    /// Example:
    ///
    /// ```
    /// use sjakk::square::{File, Rank, Square};
    ///
    /// let square = Square::from_file_and_rank(File::A, Rank::Rank1);
    /// assert_eq!(square.0, 0);
    ///
    /// ```
    pub fn from_file_and_rank(file: File, rank: Rank) -> Self {
        Self((rank as u8 * 8) + file as u8)
    }

    pub fn file(&self) -> File {
        File::from_index((self.0 & 7) as usize)
    }

    pub fn rank(&self) -> Rank {
        Rank::from_index((self.0 >> 3) as usize)
    }
}

impl From<Bitboard> for Square {
    fn from(bb: Bitboard) -> Self {
        bb.lsb_square()
    }
}

impl std::fmt::Display for Square {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.file(), self.rank())
    }
}

#[derive(Debug)]
pub struct SquareParseError;

impl std::str::FromStr for Square {
    type Err = SquareParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() < 2 {
            return Err(SquareParseError);
        }
        let ch: Vec<char> = s.chars().collect();
        match ch[0] {
            'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' => {}
            _ => {
                return Err(SquareParseError);
            }
        }
        match ch[1] {
            '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' => {}
            _ => {
                return Err(SquareParseError);
            }
        }
        Ok(Square::from_file_and_rank(
            File::from_index((ch[0] as usize) - ('a' as usize)),
            Rank::from_index((ch[1] as usize) - ('1' as usize)),
        ))
    }
}

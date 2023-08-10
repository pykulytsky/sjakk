use std::fmt::Display;

use crate::{piece::PieceType, Bitboard, Color, Rank, Square};

pub trait MoveType: PartialEq + Eq + Display {
    fn capture(&self) -> Option<(PieceType, Square)>;
    fn from(&self) -> Square;
    fn to(&self) -> Square;
    fn piece(&self) -> PieceType;
    fn update_position(
        &self,
        white_pieces: &mut [Bitboard; 6],
        black_pieces: &mut [Bitboard; 6],
        side_to_move: Color,
    ) {
        let from_bb = Bitboard(1_u64 << self.from().0);
        let to_bb = Bitboard(1_u64 << self.to().0);
        let from_to_bb = from_bb ^ to_bb;

        match side_to_move {
            Color::White => white_pieces[self.piece() as usize] ^= from_to_bb,
            Color::Black => black_pieces[self.piece() as usize] ^= from_to_bb,
        }

        if let Some((piece, _)) = self.capture() {
            match side_to_move.opposite() {
                Color::White => white_pieces[piece as usize] ^= to_bb,
                Color::Black => black_pieces[piece as usize] ^= to_bb,
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Quite {
    pub from: Square,
    pub to: Square,
    pub piece: PieceType,
    pub capture: Option<PieceType>,
}

impl MoveType for Quite {
    fn capture(&self) -> Option<(PieceType, Square)> {
        if let Some(capture) = self.capture {
            return Some((capture, self.to));
        }
        None
    }

    fn from(&self) -> Square {
        self.from
    }

    fn to(&self) -> Square {
        self.to
    }

    fn piece(&self) -> PieceType {
        self.piece
    }
}

impl Quite {
    pub fn new(from: Square, to: Square, piece: PieceType, capture: Option<PieceType>) -> Self {
        Self {
            from,
            to,
            piece,
            capture,
        }
    }
}

impl Display for Quite {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}",
            if self.piece == PieceType::Pawn {
                "".to_string()
            } else {
                format!("{}", self.piece)
            },
            if self.capture.is_some() {
                if self.piece == PieceType::Pawn {
                    format!("{}x", self.from.file())
                } else {
                    "x".to_string()
                }
            } else {
                "".to_string()
            },
            self.to,
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Promotion {
    pub from: Square,
    pub to: Square,
    pub capture: Option<PieceType>,
    pub promotion_to: PieceType,
}

impl MoveType for Promotion {
    fn capture(&self) -> Option<(PieceType, Square)> {
        if let Some(capture) = self.capture {
            return Some((capture, self.to));
        }
        None
    }

    fn from(&self) -> Square {
        self.from
    }

    fn to(&self) -> Square {
        self.to
    }

    fn piece(&self) -> PieceType {
        PieceType::Pawn
    }

    fn update_position(
        &self,
        white_pieces: &mut [Bitboard; 6],
        black_pieces: &mut [Bitboard; 6],
        side_to_move: Color,
    ) {
        let from_bb = Bitboard(1_u64 << self.from().0);
        let to_bb = Bitboard(1_u64 << self.to().0);

        match side_to_move {
            Color::White => {
                white_pieces[self.piece() as usize] ^= from_bb;
                white_pieces[self.promotion_to as usize] ^= to_bb;
            }
            Color::Black => {
                black_pieces[self.piece() as usize] ^= from_bb;
                black_pieces[self.promotion_to as usize] ^= to_bb;
            }
        }

        if let Some((piece, _)) = self.capture() {
            match side_to_move.opposite() {
                Color::White => white_pieces[piece as usize] ^= to_bb,
                Color::Black => black_pieces[piece as usize] ^= to_bb,
            }
        }
    }
}

impl Display for Promotion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}",
            if self.capture.is_some() {
                format!("{}x", self.from.file())
            } else {
                "".to_string()
            },
            self.to,
            format!("={}", self.promotion_to).to_uppercase()
        )
    }
}

impl Promotion {
    pub fn new(
        from: Square,
        to: Square,
        capture: Option<PieceType>,
        promotion_to: PieceType,
    ) -> Self {
        Self {
            from,
            to,
            capture,
            promotion_to,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct EnPassant {
    pub from: Square,
    pub to: Square,
    pub piece: PieceType,
    pub captures_on: Square,
}

impl EnPassant {
    pub fn new(from: Square, to: Square, piece: PieceType) -> Self {
        let captures_on = match to.rank() {
            Rank::Rank3 => Square::from_file_and_rank(to.file(), Rank::Rank4),
            Rank::Rank6 => Square::from_file_and_rank(to.file(), Rank::Rank5),
            _ => unreachable!(),
        };
        Self {
            from,
            to,
            piece,
            captures_on,
        }
    }
}

impl MoveType for EnPassant {
    fn capture(&self) -> Option<(PieceType, Square)> {
        Some((PieceType::Pawn, self.captures_on))
    }

    fn from(&self) -> Square {
        self.from
    }

    fn to(&self) -> Square {
        self.to
    }

    fn piece(&self) -> PieceType {
        PieceType::Pawn
    }

    fn update_position(
        &self,
        white_pieces: &mut [Bitboard; 6],
        black_pieces: &mut [Bitboard; 6],
        side_to_move: Color,
    ) {
        let from_bb = Bitboard(1_u64 << self.from().0);
        let to_bb = Bitboard(1_u64 << self.to().0);
        let from_to_bb = from_bb ^ to_bb;

        match side_to_move {
            Color::White => white_pieces[self.piece() as usize] ^= from_to_bb,
            Color::Black => black_pieces[self.piece() as usize] ^= from_to_bb,
        }

        match side_to_move.opposite() {
            Color::White => {
                white_pieces[self.piece() as usize] ^= Bitboard::from_square(self.captures_on)
            }
            Color::Black => {
                black_pieces[self.piece() as usize] ^= Bitboard::from_square(self.captures_on)
            }
        }
    }
}

impl Display for EnPassant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", format!("{}x", self.from.file()), self.to)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Castling {
    from: Square,
    to: Square,
}

impl Castling {
    pub fn new(from: Square, to: Square) -> Self {
        Self { from, to }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Move {
    pub from: Square,
    pub to: Square,
    pub piece: PieceType,
    pub capture: Option<PieceType>,
    pub promotion: Option<PieceType>,
}

impl Move {
    pub fn new(
        from: Square,
        to: Square,
        piece: PieceType,
        capture: Option<PieceType>,
        promotion: Option<PieceType>,
    ) -> Self {
        Self {
            from,
            to,
            piece,
            capture,
            promotion,
        }
    }
}

impl Display for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}{}",
            if self.piece == PieceType::Pawn {
                "".to_string()
            } else {
                format!("{}", self.piece)
            },
            if self.capture.is_some() {
                if self.piece == PieceType::Pawn {
                    format!("{}x", self.from.file())
                } else {
                    "x".to_string()
                }
            } else {
                "".to_string()
            },
            self.to,
            if let Some(promotion) = self.promotion {
                format!("={promotion}").to_uppercase()
            } else {
                "".to_string()
            }
        )
    }
}

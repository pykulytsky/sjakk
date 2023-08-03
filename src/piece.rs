#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Color {
    White,
    Black,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Piece {
    Pawn,
    Rook,
    Knight,
    Bishop,
    Queen,
    King,
}

impl std::fmt::Display for Piece {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let notation = match self {
            Piece::Pawn => "p",
            Piece::Rook => "r",
            Piece::Knight => "n",
            Piece::Bishop => "b",
            Piece::Queen => "q",
            Piece::King => "k",
        };

        write!(f, "{notation}")
    }
}

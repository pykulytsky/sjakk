use crate::{piece::PieceType, Square};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Move {
    from: Square,
    to: Square,
    capture: bool,
    promotion: Option<PieceType>,
}

impl Move {
    pub fn new(from: Square, to: Square, capture: bool, promotion: Option<PieceType>) -> Self {
        Self {
            from,
            to,
            capture,
            promotion,
        }
    }
}

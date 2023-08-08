use crate::{piece::PieceType, Square};

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

impl std::fmt::Display for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}{}",
            self.piece,
            if self.capture.is_some() { "x" } else { "" },
            self.to,
            if let Some(promotion) = self.promotion {
                format!("={promotion}")
            } else {
                "".to_string()
            }
        )
    }
}

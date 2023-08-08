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

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum CatslingRights {
    NoCastling,
    KingSide,
    QueenSide,
    Both,
}

impl CatslingRights {
    pub fn remove_queenside(&mut self) {
        match self {
            CatslingRights::QueenSide => *self = Self::NoCastling,
            CatslingRights::Both => *self = Self::KingSide,
            _ => {}
        }
    }

    pub fn remove_kingside(&mut self) {
        match self {
            CatslingRights::KingSide => *self = Self::NoCastling,
            CatslingRights::Both => *self = Self::QueenSide,
            _ => {}
        }
    }
}

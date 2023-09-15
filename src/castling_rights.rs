use crate::{board::Board, hashing, File, Move, PieceType};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum CastlingRights {
    NoCastling,
    KingSide,
    QueenSide,
    Both,
}

impl CastlingRights {
    pub fn remove_queenside(&mut self) {
        match self {
            CastlingRights::QueenSide => *self = Self::NoCastling,
            CastlingRights::Both => *self = Self::KingSide,
            _ => {}
        }
    }

    pub fn remove_kingside(&mut self) {
        match self {
            CastlingRights::KingSide => *self = Self::NoCastling,
            CastlingRights::Both => *self = Self::QueenSide,
            _ => {}
        }
    }
    pub fn is_kingside(&self) -> bool {
        *self as usize == 1 || *self as usize == 3
    }

    pub fn is_queenside(&self) -> bool {
        *self as usize == 2 || *self as usize == 3
    }

    pub fn update_castling_rights(
        board: &mut Board,
        piece: PieceType,
        captured: Option<PieceType>,
        m: &Move,
    ) {
        if board.castling_rights[board.side_to_move as usize] != CastlingRights::NoCastling {
            match piece {
                PieceType::Rook => match m.from().file() {
                    File::A
                        if board.castling_rights[board.side_to_move as usize].is_queenside() =>
                    {
                        board.castling_rights[board.side_to_move as usize].remove_queenside();
                        board.hash ^= hashing::CASTLE_KEYS[board.side_to_move as usize]
                            [board.castling_rights[board.side_to_move as usize] as usize];
                    }
                    File::H if board.castling_rights[board.side_to_move as usize].is_kingside() => {
                        board.castling_rights[board.side_to_move as usize].remove_kingside();
                        board.hash ^= hashing::CASTLE_KEYS[board.side_to_move as usize]
                            [board.castling_rights[board.side_to_move as usize] as usize];
                    }
                    _ => {}
                },
                PieceType::King => {
                    board.castling_rights[board.side_to_move as usize] = CastlingRights::NoCastling;
                    board.hash ^= hashing::CASTLE_KEYS[board.side_to_move as usize][0];
                }
                _ => {}
            }
        }
        if board.castling_rights[board.side_to_move.opposite() as usize]
            != CastlingRights::NoCastling
        {
            if let Some(PieceType::Rook) = captured {
                match m.to().file() {
                    File::A
                        if board.castling_rights[board.side_to_move.opposite() as usize]
                            .is_queenside() =>
                    {
                        board.castling_rights[board.side_to_move.opposite() as usize]
                            .remove_queenside();
                        board.hash ^= hashing::CASTLE_KEYS[board.side_to_move.opposite() as usize]
                            [board.castling_rights[board.side_to_move as usize] as usize];
                    }
                    File::H
                        if board.castling_rights[board.side_to_move.opposite() as usize]
                            .is_kingside() =>
                    {
                        board.castling_rights[board.side_to_move.opposite() as usize]
                            .remove_kingside();
                        board.hash ^= hashing::CASTLE_KEYS[board.side_to_move.opposite() as usize]
                            [board.castling_rights[board.side_to_move as usize] as usize];
                    }
                    _ => {}
                }
            }
        }
    }
}

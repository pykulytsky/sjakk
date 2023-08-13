use std::{num::ParseIntError, str::FromStr};

use crate::{
    board::Board,
    piece::{Color, PieceType},
    Bitboard, Square,
};
use strum::IntoEnumIterator;
use thiserror::Error;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct FEN {
    pub pieces: [[Bitboard; 6]; 2],
    pub active_color: Color,
    pub en_passant_target: Option<Square>,
    pub halfmove_clock: u16,
    pub fullmove_number: u16,
}

#[derive(Error, Debug)]
pub enum FENParseError {
    #[error("not enough parts in FEN notation")]
    NotEnoughParts,
    #[error("invalid piece placement")]
    PiecePlacement,
    #[error("invalid active color")]
    ActiveColor,
    #[error("invalid halfmove clock")]
    HalfmoveClock(#[from] ParseIntError),
    #[error("invalid fullmove number")]
    FullmoveNumber,
}

pub fn parse(fen: &str) -> Result<FEN, FENParseError> {
    FEN::from_str(fen)
}

impl FromStr for FEN {
    type Err = FENParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let fen = s.trim().replace("/", "");
        let mut parts = fen.split(" ");
        assert!(fen.split(" ").clone().count() == 6);

        let pieces = parse_piece_placement(parts.next().unwrap())?;
        let active_color = parse_active_color(parts.next().unwrap())?;
        // Skip castling rights and posible en passant for now.
        let _ = parts.next();
        let en_passant_target = parse_en_passant_target(parts.next().unwrap());
        let (halfmove_clock, fullmove_number) = parse_moves(parts.take(2))?;

        Ok(FEN {
            pieces,
            active_color,
            en_passant_target,
            halfmove_clock,
            fullmove_number,
        })
    }
}

fn parse_moves(
    mut take: std::iter::Take<std::str::Split<'_, &str>>,
) -> Result<(u16, u16), FENParseError> {
    // TODO: fix errors convertation
    Ok((
        take.next()
            .ok_or_else(|| FENParseError::FullmoveNumber)?
            .parse::<u16>()?,
        take.next()
            .ok_or_else(|| FENParseError::FullmoveNumber)?
            .parse::<u16>()?,
    ))
}

fn parse_active_color(color: &str) -> Result<Color, FENParseError> {
    match color {
        "b" => Ok(Color::Black),
        "w" => Ok(Color::White),
        _ => Err(FENParseError::ActiveColor),
    }
}

/// Parses piece placement part of FEN.
/// Since in FEN notation goes from 8 rank to 1 rank, we flip resulting [`Bitboard`]'s vertically,
/// to fit internal [`Bitboard`] representation.
fn parse_piece_placement(notation: &str) -> Result<[[Bitboard; 6]; 2], FENParseError> {
    let mut offset = 0;
    let mut pieces_bb = [
        [
            Bitboard(0),
            Bitboard(0),
            Bitboard(0),
            Bitboard(0),
            Bitboard(0),
            Bitboard(0),
        ],
        [
            Bitboard(0),
            Bitboard(0),
            Bitboard(0),
            Bitboard(0),
            Bitboard(0),
            Bitboard(0),
        ],
    ];
    for n in notation.chars() {
        if n.is_digit(10) {
            offset += n
                .to_digit(10)
                .ok_or_else(|| FENParseError::PiecePlacement)?;
        } else {
            match n {
                'p' => {
                    pieces_bb[Color::Black as usize][PieceType::Pawn as usize] |=
                        Bitboard(1_u64 << offset);
                }
                'P' => {
                    pieces_bb[Color::White as usize][PieceType::Pawn as usize] |=
                        Bitboard(1_u64 << offset);
                }
                'r' => {
                    pieces_bb[Color::Black as usize][PieceType::Rook as usize] |=
                        Bitboard(1_u64 << offset);
                }
                'R' => {
                    pieces_bb[Color::White as usize][PieceType::Rook as usize] |=
                        Bitboard(1_u64 << offset);
                }
                'n' => {
                    pieces_bb[Color::Black as usize][PieceType::Knight as usize] |=
                        Bitboard(1_u64 << offset);
                }
                'N' => {
                    pieces_bb[Color::White as usize][PieceType::Knight as usize] |=
                        Bitboard(1_u64 << offset);
                }
                'b' => {
                    pieces_bb[Color::Black as usize][PieceType::Bishop as usize] |=
                        Bitboard(1_u64 << offset);
                }
                'B' => {
                    pieces_bb[Color::White as usize][PieceType::Bishop as usize] |=
                        Bitboard(1_u64 << offset);
                }
                'q' => {
                    pieces_bb[Color::Black as usize][PieceType::Queen as usize] |=
                        Bitboard(1_u64 << offset);
                }
                'Q' => {
                    pieces_bb[Color::White as usize][PieceType::Queen as usize] |=
                        Bitboard(1_u64 << offset);
                }
                'k' => {
                    pieces_bb[Color::Black as usize][PieceType::King as usize] |=
                        Bitboard(1_u64 << offset);
                }
                'K' => {
                    pieces_bb[Color::White as usize][PieceType::King as usize] |=
                        Bitboard(1_u64 << offset);
                }
                _ => return Err(FENParseError::PiecePlacement),
            }
            offset += 1;
        }
    }

    pieces_bb[0]
        .iter_mut()
        .for_each(|p| *p = Bitboard(p.0.swap_bytes()));
    pieces_bb[1]
        .iter_mut()
        .for_each(|p| *p = Bitboard(p.0.swap_bytes()));

    Ok(pieces_bb)
}

fn parse_en_passant_target(input: &str) -> Option<Square> {
    match input {
        "-" => None,
        _ => Square::from_str(input).ok(),
    }
}

impl From<Board> for FEN {
    fn from(board: Board) -> Self {
        Self {
            pieces: [board.white_pieces, board.black_pieces],
            active_color: board.side_to_move,
            en_passant_target: None,
            halfmove_clock: board.move_list.len() as u16,
            fullmove_number: (board.move_list.len() / 2) as u16,
        }
    }
}

impl ToString for FEN {
    fn to_string(&self) -> String {
        let mut pieces_placement = "".to_string();
        let white = self.pieces[0]
            .into_iter()
            .reduce(|acc, next| acc | next)
            .unwrap();
        let black = self.pieces[1]
            .into_iter()
            .reduce(|acc, next| acc | next)
            .unwrap();
        let pieces = (white.0 | black.0).swap_bytes();

        let mut offset = 0;

        for i in 0..64 {
            if i != 0 && i % 8 == 0 {
                if offset != 0 {
                    pieces_placement += offset.to_string().as_str();
                }

                offset = 0;
                pieces_placement += "/";
            }
            let mut empty = true;
            for (side, piece) in self.pieces.iter().enumerate() {
                for p in PieceType::iter() {
                    if pieces & (1_u64 << i) & piece[p as usize].0.swap_bytes() != 0 {
                        empty = false;

                        if offset != 0 {
                            pieces_placement += offset.to_string().as_str();
                        }

                        offset = 0;
                        match (side, p) {
                            (0, PieceType::Pawn) => {
                                pieces_placement += "P";
                            }
                            (1, PieceType::Pawn) => {
                                pieces_placement += "p";
                            }
                            (0, PieceType::Rook) => {
                                pieces_placement += "R";
                            }
                            (1, PieceType::Rook) => {
                                pieces_placement += "r";
                            }
                            (0, PieceType::Bishop) => {
                                pieces_placement += "B";
                            }
                            (1, PieceType::Bishop) => {
                                pieces_placement += "b";
                            }
                            (0, PieceType::Knight) => {
                                pieces_placement += "N";
                            }
                            (1, PieceType::Knight) => {
                                pieces_placement += "n";
                            }
                            (0, PieceType::Queen) => {
                                pieces_placement += "Q";
                            }
                            (1, PieceType::Queen) => {
                                pieces_placement += "q";
                            }
                            (0, PieceType::King) => {
                                pieces_placement += "K";
                            }
                            (1, PieceType::King) => {
                                pieces_placement += "k";
                            }
                            _ => {}
                        }
                        break;
                    }
                }
            }
            if empty {
                offset += 1;
            }
        }

        if offset != 0 {
            pieces_placement += offset.to_string().as_str();
        }
        let en_passant_target = match self.en_passant_target {
            Some(sq) => sq.to_string(),
            None => "-".to_string(),
        };
        let castling_rights = "-";
        format!(
            "{} {} {} {} {} {}",
            pieces_placement,
            self.active_color.to_string(),
            castling_rights,
            en_passant_target,
            self.halfmove_clock,
            self.fullmove_number
        )
    }
}

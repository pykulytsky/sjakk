use std::num::ParseIntError;

use crate::{
    piece::{Color, PieceType},
    Bitboard, Square,
};
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
    let fen = fen.trim().replace("/", "");
    let mut parts = fen.split(" ");
    assert!(fen.split(" ").clone().count() == 6);

    let pieces = parse_piece_placement(parts.next().unwrap())?;
    let active_color = parse_active_color(parts.next().unwrap())?;
    // Skip castling rights and posible en passant for now.
    let _ = parts.next();
    let _ = parts.next();
    let (halfmove_clock, fullmove_number) = parse_moves(parts.take(2))?;

    Ok(FEN {
        pieces,
        active_color,
        en_passant_target: None,
        halfmove_clock,
        fullmove_number,
    })
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

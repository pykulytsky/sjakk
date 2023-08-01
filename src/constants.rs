//! Provides some useful constants, that are used throughout the crate.
use strum_macros::EnumIter;
/// Represents all possible directions in chess, that piece can move in.
///
///  northwest    north   northeast
///  noWe         nort         noEa
///  `       
///              \  |  /
///  west       <-  0 ->       east
///              /  |  \
///
///  soWe         sout         soEa
///  southwest    south   southeast
///  `
#[derive(Debug, EnumIter)]
pub enum Direction {
    North,
    NorthEast,
    East,
    SouthEast,
    South,
    SouthWest,
    West,
    NorthWest,
}

pub const CLEAR_RANK: [u64; 8] = [
    u64::MAX << 8,
    u64::MAX << 16 | 0xFF_u64,
    (u64::MAX << 16 | 0xFF_u64) << 8 | 0xFF_u64,
    (u64::MAX << 16 | 0xFF_u64) << 16 | 0xFFFF_u64,
    (u64::MAX >> 16 | 0xFF_u64.reverse_bits()) >> 16 | 0xFFFF_u64.reverse_bits(),
    (u64::MAX >> 16 | 0xFF_u64.reverse_bits()) >> 8 | 0xFF_u64.reverse_bits(),
    u64::MAX >> 16 | 0xFF_u64.reverse_bits(),
    u64::MAX >> 8,
];

pub const MASK_RANK: [u64; 8] = [
    !CLEAR_RANK[0],
    !CLEAR_RANK[1],
    !CLEAR_RANK[2],
    !CLEAR_RANK[3],
    !CLEAR_RANK[4],
    !CLEAR_RANK[5],
    !CLEAR_RANK[6],
    !CLEAR_RANK[7],
];

const FILE: u64 = u64::from_ne_bytes([
    0b00000001, 0b00000001, 0b00000001, 0b00000001, 0b00000001, 0b00000001, 0b00000001, 0b00000001,
]);

pub const MASK_FILE: [u64; 8] = [
    FILE << 0 as u8,
    FILE << 1 as u8,
    FILE << 2 as u8,
    FILE << 3 as u8,
    FILE << 4 as u8,
    FILE << 5 as u8,
    FILE << 6 as u8,
    FILE << 7 as u8,
];

pub const CLEAR_FILE: [u64; 8] = [
    !MASK_FILE[0],
    !MASK_FILE[1],
    !MASK_FILE[2],
    !MASK_FILE[3],
    !MASK_FILE[4],
    !MASK_FILE[5],
    !MASK_FILE[6],
    !MASK_FILE[7],
];

pub const INDEX64: [u64; 64] = [
    0, 1, 48, 2, 57, 49, 28, 3, 61, 58, 50, 42, 38, 29, 17, 4, 62, 55, 59, 36, 53, 51, 43, 22, 45,
    39, 33, 30, 24, 18, 12, 5, 63, 47, 56, 27, 60, 41, 37, 16, 54, 35, 52, 21, 44, 32, 23, 11, 46,
    26, 40, 15, 34, 20, 31, 10, 25, 14, 19, 9, 13, 8, 7, 6,
];

pub const LSB_64_TABLE: [u64; 64] = [
    63, 30, 3, 32, 59, 14, 11, 33, 60, 24, 50, 9, 55, 19, 21, 34, 61, 29, 2, 53, 51, 23, 41, 18,
    56, 28, 1, 43, 46, 27, 0, 35, 62, 31, 58, 4, 5, 49, 54, 6, 15, 52, 12, 40, 7, 42, 45, 16, 25,
    57, 48, 13, 10, 39, 8, 44, 20, 47, 38, 22, 17, 37, 36, 26,
];

pub const DEBRUIJ_T: &'static [u8] = &[
    0, 47, 1, 56, 48, 27, 2, 60, 57, 49, 41, 37, 28, 16, 3, 61, 54, 58, 35, 52, 50, 42, 21, 44, 38,
    32, 29, 23, 17, 11, 4, 62, 46, 55, 26, 59, 40, 36, 15, 53, 34, 51, 20, 43, 31, 22, 10, 45, 25,
    39, 14, 33, 19, 30, 9, 24, 13, 18, 8, 12, 7, 6, 5, 63,
];

pub const DEBRUIJ_M: u64 = 0x03f7_9d71_b4cb_0a89;

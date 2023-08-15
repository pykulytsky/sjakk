use crate::utils::{lower_ones, upper_ones};
use crate::Direction::*;

pub const RAY_ATTACKS: [[u64; 8]; 64] = ray_attacks();

#[inline]
pub const fn ray_attacks() -> [[u64; 8]; 64] {
    let mut rays = [[0u64; 8]; 64];

    let mut north = 0x0101010101010100_u64;
    let mut sq = 0;
    while sq < 64 {
        rays[sq][North as usize] = north;
        north <<= 1;
        let one = 1_u64;
        rays[sq][East as usize] = 2 * ((one << (sq | 7)) - (one << sq));
        rays[sq][West as usize] = (one << sq) - (one << (sq & 56));
        rays[sq][South as usize] = 0x0080808080808080_u64 >> (sq ^ 63);
        (rays[sq][SouthWest as usize], rays[sq][NorthEast as usize]) = ray_attacks_diag(sq);
        (rays[sq][SouthEast as usize], rays[sq][NorthWest as usize]) = ray_attacks_antidiag(sq);
        sq += 1;
    }
    rays
}

#[inline]
pub const fn ray_attacks_diag(sq: usize) -> (u64, u64) {
    let maindia = 0x8040201008040201_u64;
    let diag = (8 * (sq as isize & 7)).wrapping_sub(sq as isize & 56);
    let north = -diag & (diag >> 31);
    let south = diag & (-diag >> 31);
    let diags = (maindia >> south) << north;
    let south_west = lower_ones(sq as u8) & diags;
    let north_east = upper_ones(sq as u8) & diags;
    (south_west, north_east)
}

#[inline]
pub const fn ray_attacks_antidiag(sq: usize) -> (u64, u64) {
    let maindia = 0x0102040810204080_u64;
    let antidiag: isize = 7 - (sq as isize & 7) - (sq as isize >> 3);
    let antidiags = if antidiag >= 0 {
        maindia >> (antidiag * 8)
    } else {
        maindia << (-antidiag * 8)
    };

    let south_east = lower_ones(sq as u8) & antidiags;
    let north_west = upper_ones(sq as u8) & antidiags;

    (south_east, north_west)
}

use crate::constants::{DEBRUIJ_M, DEBRUIJ_T};

pub const BITSCAN_FORWARD: [usize; 4] = [0, 1, 2, 7];
pub const BITSCAN_REVERSE: [usize; 4] = [3, 4, 5, 6];

#[inline(always)]
pub fn bit_scan_forward(bits: u64) -> u8 {
    assert_ne!(bits, 0);
    DEBRUIJ_T[(((bits ^ bits.wrapping_sub(1)).wrapping_mul(DEBRUIJ_M)).wrapping_shr(58)) as usize]
}

#[inline(always)]
pub fn bit_scan_reverse(mut bits: u64) -> u8 {
    assert_ne!(bits, 0);
    bits |= bits >> 1;
    bits |= bits >> 2;
    bits |= bits >> 4;
    bits |= bits >> 8;
    bits |= bits >> 16;
    bits |= bits >> 32;
    unsafe { *DEBRUIJ_T.get_unchecked((bits.wrapping_mul(DEBRUIJ_M)).wrapping_shr(58) as usize) }
}

#[inline]
pub const fn upper_ones(square: u8) -> u64 {
    !1_u64 << square
}

#[inline]
pub const fn lower_ones(square: u8) -> u64 {
    (1_u64 << square) - 1
}

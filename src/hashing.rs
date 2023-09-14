const SEED: u128 = 0x8F2D7BAC4E1F9089327465A0FEBD13D6;

pub struct XorShiftState {
    pub state: u128,
}

impl XorShiftState {
    pub const fn new() -> Self {
        Self { state: SEED }
    }

    pub const fn next_self(mut self) -> (u64, Self) {
        let mut x = self.state;
        x ^= x >> 12;
        x ^= x << 25;
        x ^= x >> 27;
        self.state = x;
        #[allow(clippy::cast_possible_truncation)]
        let r = x as u64;
        let r = r ^ (x >> 64) as u64;
        (r, self)
    }

    pub fn gen_next(&mut self) -> u64 {
        let mut x = self.state;
        x ^= x >> 12;
        x ^= x << 25;
        x ^= x >> 27;
        self.state = x;
        #[allow(clippy::cast_possible_truncation)]
        let r = x as u64;
        r ^ (x >> 64) as u64
    }

    pub fn random_few_bits(&mut self) -> u64 {
        let first = self.gen_next();
        let second = self.gen_next();
        let third = self.gen_next();

        first & second & third
    }
}

const fn generate_zobrist_hash() -> ([[u64; 64]; 12], [u64; 8], [[u64; 4]; 2], u64) {
    let mut state = XorShiftState::new();
    let mut piece_keys = [[0; 64]; 12];
    let mut i = 0;
    while i < 12 {
        let mut square = 0;
        while square < 64 {
            let key;
            (key, state) = state.next_self();
            piece_keys[i][square] = key;
            square += 1;
        }
        i += 1;
    }
    let mut castle_keys = [[0; 4]; 2];
    let mut i = 0;
    while i < 2 {
        let mut rule = 0;
        while rule < 4 {
            let key;
            (key, state) = state.next_self();
            castle_keys[i][rule] = key;
            rule += 1;
        }
        i += 1;
    }
    let mut ep_keys = [0; 8];
    let mut i = 0;
    while i < 8 {
        let key;
        (key, state) = state.next_self();
        ep_keys[i] = key;
        i += 1;
    }
    let key;
    (key, _) = state.next_self();
    let side_key = key;
    (piece_keys, ep_keys, castle_keys, side_key)
}

pub const PIECE_KEYS: [[u64; 64]; 12] = generate_zobrist_hash().0;
pub const EP_KEYS: [u64; 8] = generate_zobrist_hash().1;
pub const CASTLE_KEYS: [[u64; 4]; 2] = generate_zobrist_hash().2;
pub const SIDE_KEY: u64 = generate_zobrist_hash().3;

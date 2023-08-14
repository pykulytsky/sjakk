use criterion::{black_box, criterion_group, criterion_main, Criterion};
use sjakk::board::Board;

fn legal_moves_in_starting_position(c: &mut Criterion) {
    c.bench_function("legal moves generation", |b| {
        let mut board = black_box(Board::default());
        b.iter(|| board.legal_moves());
    });
}

criterion_group!(benches, legal_moves_in_starting_position);
criterion_main!(benches);

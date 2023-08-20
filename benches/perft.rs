use criterion::{black_box, criterion_group, criterion_main, Criterion};
use sjakk::board::{perft, Board};

fn perft_bench(c: &mut Criterion) {
    c.bench_function("perft", |b| {
        let mut board = black_box(Board::default());
        b.iter(|| perft(&mut board, 4));
    });
}

criterion_group!(benches, perft_bench);
criterion_main!(benches);

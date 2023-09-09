use criterion::{black_box, criterion_group, criterion_main, Criterion};
use futures::executor::ThreadPool;
use sjakk::board::Board;

fn legal_moves_in_starting_position(c: &mut Criterion) {
    c.bench_function("legal moves generation", |b| {
        let board = black_box(Board::default());
        b.iter(|| board.legal_moves());
    });
}

fn negamax(c: &mut Criterion) {
    let mut group = c.benchmark_group("negamax");
    group.bench_function("sequential", |b| {
        let mut board = black_box(Board::default());
        b.iter(|| board.negamax_root(3));
    });
    group.bench_function("parallel", |b| {
        let mut board = black_box(Board::default());
        let thread_pool = ThreadPool::new().unwrap();
        b.iter(|| board.negamax_root_async(3, &thread_pool));
    });
}

criterion_group!(benches, legal_moves_in_starting_position, negamax);
criterion_main!(benches);

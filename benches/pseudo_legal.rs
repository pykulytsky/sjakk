use criterion::{black_box, criterion_group, criterion_main, Criterion};
use sjakk::PieceType;
use strum::IntoEnumIterator;

fn bench_pseudo_legal_moves(c: &mut Criterion) {
    let mut group = c.benchmark_group("Pseudo legal moves");
    let square = black_box(sjakk::Square(28));
    let occupied = black_box(sjakk::Bitboard(0x00f0f000f));
    let own = black_box(sjakk::Bitboard(0x00f0f000f));
    for piece in PieceType::iter() {
        group.bench_function(piece.to_string(), |b| {
            b.iter(|| {
                piece.pseudo_legal_moves(square, black_box(sjakk::Color::White), occupied, own)
            })
        });
    }
    group.finish();
}

criterion_group!(benches, bench_pseudo_legal_moves);
criterion_main!(benches);

use std::collections::BTreeMap;

use clap::Parser;
use futures::executor::ThreadPoolBuilder;
use sjakk::{board::Board, perft::*, Move};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    fen: String,
    #[arg(short, long)]
    depth: usize,
    #[arg(short, long, default_value_t = false)]
    split: bool,
    #[arg(short, long, default_value_t = 1)]
    threads: usize,
}

fn print_move_list(list: &BTreeMap<Move, usize>) {
    for (m, nodes) in list.iter() {
        println!("{m}: {nodes}");
    }
}

fn main() {
    let args = Cli::parse();
    let mut board = Board::from_fen(&args.fen).unwrap();
    if args.threads > 1 {
        let executor = ThreadPoolBuilder::new()
            .pool_size(args.threads)
            .create()
            .unwrap();
        if args.split {
            let (list, nodes) = perft_async_divide(&board, args.depth, &executor);
            print_move_list(&list);
            println!("\nSearched {nodes} nodes.")
        } else {
            let nodes = perft_async(&board, args.depth, &executor);
            println!("\nSearched {nodes} nodes.")
        }
    } else if args.split {
        let (list, nodes) = perft_divide(&mut board, args.depth);
        print_move_list(&list);
        println!("\nSearched {nodes} nodes.")
    } else {
        let nodes = perft(&mut board, args.depth);
        println!("\nSearched {nodes} nodes.")
    }
}

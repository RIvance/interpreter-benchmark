mod core;
mod parser;

use clap::Parser;
use std::fs;
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "stlc-interpreter")]
#[command(about = "Simply Typed Lambda Calculus Interpreter", long_about = None)]
struct Args {
    /// Input file
    #[arg(value_name = "FILE")]
    file: PathBuf,

    /// Use trampolined evaluation
    #[arg(short, long)]
    trampolined: bool,

    /// Show the parsed expression before evaluation
    #[arg(short, long)]
    verbose: bool,
}

fn main() {
    let args = Args::parse();

    // Read the input file
    let input = match fs::read_to_string(&args.file) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", args.file.display(), e);
            std::process::exit(1);
        }
    };

    // Parse the expression
    let expr = match parser::parse(&input) {
        Ok(expr) => expr,
        Err(e) => {
            eprintln!("Parse error: {}", e);
            std::process::exit(1);
        }
    };

    if args.verbose {
        println!("Parsed expression:");
        println!("{:#?}\n", expr);
    }

    // Type check the expression
    let type_ctx = core::TypeContext::new();
    let expr_type = match core::type_of(&expr, &type_ctx) {
        Ok(ty) => ty,
        Err(e) => {
            eprintln!("Type error: {}", e);
            std::process::exit(1);
        }
    };

    if args.verbose {
        println!("Expression type: {:?}\n", expr_type);
    }

    // Evaluate the expression
    let result = if args.trampolined {
        if args.verbose {
            println!("Using trampolined evaluation...");
        }
        core::evaluate_named_trampolined(&expr)
    } else {
        if args.verbose {
            println!("Using direct evaluation...");
        }
        core::evaluate_named(&expr)
    };

    // Print the result
    println!("Result: {:?}", result);
}
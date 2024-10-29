use clap::{Parser, Subcommand};
use jus_rs::Jus;
use std::path::PathBuf;

#[derive(Debug, Parser)]
#[command(name = "jus")]
#[command(about = "A tool for working with JUS (JSON Understated Schema) files")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Debug, Subcommand)]
enum Commands {
    #[command(name = "fmt")]
    Fmt {
        input: PathBuf,
    },
    Validate {
        schema: PathBuf,
        json: PathBuf,
    },
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Fmt { input } => {
            let input_str = std::fs::read_to_string(input.clone())?;
            let jus = Jus::compile(&input_str)?;
            let output = jus.decompile();
            std::fs::write(input, output)?;
        }
        Commands::Validate { schema, json } => {
            let schema_str = std::fs::read_to_string(schema)?;
            let json_str = std::fs::read_to_string(json)?;
            let jus = Jus::compile(&schema_str)?;
            let json = serde_json::from_str(&json_str)?;
            if jus.validate(&json) {
                println!("Valid");
            } else {
                println!("Invalid");
            }
        }
    }

    Ok(())
}

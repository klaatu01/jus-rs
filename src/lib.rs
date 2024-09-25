use types::{Jus, JusError};

mod parser;
mod types;

pub fn parse(input: &str) -> Result<Jus, JusError> {
    Ok(Jus {})
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let input = include_str!("../samples/simple.jus");
        let result = parse(input);
        assert!(result.is_ok());
    }
}

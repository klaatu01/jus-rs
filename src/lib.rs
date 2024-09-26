use compiler::validator::Validator;

mod compiler;
mod parser;

#[derive(Debug, thiserror::Error)]
pub enum JusError {
    #[error("Parse error: {0}")]
    ParseError(#[from] parser::ParseError),
}

pub struct Jus {
    pub(crate) validators: Vec<Validator>,
}

impl Jus {
    pub fn validate(&self, value: &serde_json::Value) -> bool {
        self.validators
            .iter()
            .all(|validator| validator.validate(value))
    }

    pub fn compile(input: &str) -> Result<Jus, JusError> {
        let ast = parser::parse_ast(input)?;
        let validators = compiler::compile_ast(ast);
        Ok(Jus { validators })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile() {
        let input = include_str!("../samples/simple.jus");
        let result = Jus::compile(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_and_validate() {
        let input = include_str!("../samples/simple.jus");
        let jus = Jus::compile(input).unwrap();
        let value = serde_json::json!({
            "hello": "Hello",
            "world": "World",
        });
        assert!(jus.validate(&value));

        let value = serde_json::json!({
            "hello": "Hello",
        });
        assert!(jus.validate(&value));
    }

    #[test]
    fn test_compile_and_validate_fail() {
        let input = include_str!("../samples/simple.jus");
        let jus = Jus::compile(input).unwrap();
        let value = serde_json::json!({
            "hello": "Hello",
            "world": 42,
        });
        assert!(!jus.validate(&value));
    }

    #[test]
    fn test_compile_complex() {
        let input = include_str!("../samples/complex.jus");
        let result = Jus::compile(input);
        assert!(result.is_ok());

        let jus = result.unwrap();

        println!("{:?}", jus.validators);

        let value = serde_json::json!({
            "id": "123",
            "name": "John Doe",
            "username": "johndoe",
            "contactInfo": {
                "email": "test@test.com",
                "phone": "123-456-7890",
                "socialMedia": {
                    "twitter": "@johndoe",
                    "linkedin": "johndoe",
                    "github": "johndoe",
                },
            },
            "friends": ["456", "789"],
            "status": "active",
            "bio": "Hello, world!",
            "preferences": {
                "theme": "light",
                "notifications": {
                    "email": true,
                    "sms": false,
                    "push": true,
                },
            },
        });
        assert!(jus.validate(&value));
    }
}

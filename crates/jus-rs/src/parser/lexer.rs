use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, anychar, char, digit1, multispace0, one_of},
    combinator::{opt, recognize},
    sequence::tuple,
    IResult,
};
use nom_locate::{position, LocatedSpan};

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, Clone, thiserror::Error, PartialEq, Eq)]
pub enum LexicalError {
    #[error("Invalid token at line {line}, column {column}: {message}")]
    InvalidToken {
        line: u32,
        column: usize,
        message: String,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub position: Span<'a>,
}

impl<'a> Token<'a> {
    pub fn token_type(&self) -> &TokenType {
        &self.token_type
    }
    pub fn width(&self) -> usize {
        self.position.fragment().len()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub enum TokenType {
    Identifier(String),
    StringLiteral(String),
    BoolLiteral(String),
    NumberLiteral(String),
    Colon,
    Optional,
    Union,
    Nullable,
    ArrayStart,
    ArrayEnd,
    CurlyStart,
    CurlyEnd,
    BracketStart,
    BracketEnd,
    SemiColon,
}

fn parse_symbol(input: Span, symbol: String, token_type: TokenType) -> IResult<Span, Token> {
    let (input, position) = position(input)?;
    let (input, _) = tag(symbol.as_str())(input)?;
    Ok((
        input,
        Token {
            token_type,
            position,
        },
    ))
}

fn parse_colon(input: Span) -> IResult<Span, Token> {
    parse_symbol(input, ":".to_string(), TokenType::Colon)
}

fn parse_optional(input: Span) -> IResult<Span, Token> {
    parse_symbol(input, "?".to_string(), TokenType::Optional)
}

fn parse_union(input: Span) -> IResult<Span, Token> {
    parse_symbol(input, "|".to_string(), TokenType::Union)
}

fn parse_nullable(input: Span) -> IResult<Span, Token> {
    parse_symbol(input, "!".to_string(), TokenType::Nullable)
}

fn parse_array_start(input: Span) -> IResult<Span, Token> {
    parse_symbol(input, "[".to_string(), TokenType::ArrayStart)
}

fn parse_array_end(input: Span) -> IResult<Span, Token> {
    parse_symbol(input, "]".to_string(), TokenType::ArrayEnd)
}

fn parse_curly_start(input: Span) -> IResult<Span, Token> {
    parse_symbol(input, "{".to_string(), TokenType::CurlyStart)
}

fn parse_curly_end(input: Span) -> IResult<Span, Token> {
    parse_symbol(input, "}".to_string(), TokenType::CurlyEnd)
}

fn parse_bracket_start(input: Span) -> IResult<Span, Token> {
    parse_symbol(input, "(".to_string(), TokenType::BracketStart)
}

fn parse_bracket_end(input: Span) -> IResult<Span, Token> {
    parse_symbol(input, ")".to_string(), TokenType::BracketEnd)
}

fn parse_semi_colon(input: Span) -> IResult<Span, Token> {
    parse_symbol(input, ";".to_string(), TokenType::SemiColon)
}

fn parse_identifier(input: Span) -> IResult<Span, Token> {
    let (input, position) = position(input)?;
    let (input, identifier) = alpha1(input)?;
    Ok((
        input,
        Token {
            token_type: TokenType::Identifier(identifier.to_string()),
            position,
        },
    ))
}

fn parse_string_literal(input: Span) -> IResult<Span, Token> {
    let (input, position) = position(input)?;
    let (input, _) = tag("\"")(input)?;
    let (input, string) = alpha1(input)?;
    let (input, _) = tag("\"")(input)?;
    Ok((
        input,
        Token {
            token_type: TokenType::StringLiteral(string.to_string()),
            position,
        },
    ))
}

fn parse_number_literal(input: Span) -> IResult<Span, Token> {
    let (input, number_str) = recognize(number)(input)?;
    // Safely parse the number, handling potential errors
    match number_str.fragment().parse::<f64>() {
        Ok(number) => Ok((
            input,
            Token {
                token_type: TokenType::NumberLiteral(number.to_string()),
                position: number_str,
            },
        )),
        Err(_) => Err(nom::Err::Error(nom::error::Error::new(
            number_str,
            nom::error::ErrorKind::Float,
        ))),
    }
}

fn number(input: Span) -> IResult<Span, Span> {
    recognize(tuple((
        opt(sign),
        alt((
            // Floats with digits before and after the decimal point
            recognize(tuple((digit1, char('.'), digit1, opt(exponent)))),
            // Floats starting with a decimal point (e.g., .5)
            recognize(tuple((char('.'), digit1, opt(exponent)))),
            // Floats ending with a decimal point (e.g., 2.)
            recognize(tuple((digit1, char('.'), opt(exponent)))),
            // Integers with exponent
            recognize(tuple((digit1, exponent))),
            // Integers
            digit1,
        )),
    )))(input)
}

fn sign(input: Span) -> IResult<Span, char> {
    one_of("+-")(input)
}

fn exponent(input: Span) -> IResult<Span, Span> {
    recognize(tuple((one_of("eE"), opt(sign), digit1)))(input)
}

fn parse_bool_literal(input: Span) -> IResult<Span, Token> {
    let (input, position) = position(input)?;
    let (input, bool_literal) = alt((tag("true"), tag("false")))(input)?;
    Ok((
        input,
        Token {
            token_type: TokenType::BoolLiteral(bool_literal.to_string()),
            position,
        },
    ))
}

fn skip_whitespace(input: Span) -> IResult<Span, ()> {
    let (input, _) = position(input)?;
    let (input, _) = multispace0(input)?;
    Ok((input, ()))
}

fn parse_token(input: Span) -> IResult<Span, Option<Token>> {
    let (input, _) = skip_whitespace(input)?;
    if input.is_empty() {
        return Ok((input, None));
    }
    let (input, token) = alt((
        parse_colon,
        parse_optional,
        parse_union,
        parse_nullable,
        parse_array_start,
        parse_array_end,
        parse_curly_start,
        parse_curly_end,
        parse_bracket_start,
        parse_bracket_end,
        parse_semi_colon,
        parse_bool_literal,
        parse_number_literal,
        parse_string_literal,
        parse_identifier,
    ))(input)?;
    Ok((input, Some(token)))
}

pub fn lex(input: &str) -> Result<Vec<Token>, Vec<LexicalError>> {
    let mut tokens = Vec::new();
    let mut errors = Vec::new();
    let mut input = Span::new(input);
    while !input.fragment().is_empty() {
        match parse_token(input) {
            Ok((rest, Some(token))) => {
                tokens.push(token);
                input = rest;
            }
            Ok((_, None)) => {
                break;
            }
            Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                errors.push(LexicalError::InvalidToken {
                    line: e.input.location_line(),
                    column: e.input.get_column(),
                    message: e.input.fragment().chars().next().unwrap().to_string(),
                });
                // Skip one character and update position correctly
                match anychar::<Span, ()>(e.input) {
                    Ok((rest, _)) => input = rest,
                    Err(_) => {
                        break;
                    }
                }
            }
            Err(_) => {
                errors.push(LexicalError::InvalidToken {
                    line: 0,
                    column: 0,
                    message: "Unknown error".to_string(),
                });
                break;
            }
        }
    }
    if errors.is_empty() {
        Ok(tokens)
    } else {
        Err(errors)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_colon() {
        let input = Span::new(":");
        let result = parse_colon(input);
        assert!(result.is_ok());
        let (rest, token) = result.unwrap();
        assert!(rest.fragment().is_empty());
        match token.token_type {
            TokenType::Colon => (),
            _ => panic!("Expected Colon token"),
        }
    }

    #[test]
    fn test_parse_optional() {
        let input = Span::new("?");
        let result = parse_optional(input);
        assert!(result.is_ok());
        let (rest, token) = result.unwrap();
        assert!(rest.fragment().is_empty());
        match token.token_type {
            TokenType::Optional => (),
            _ => panic!("Expected Optional token"),
        }
    }

    #[test]
    fn test_parse_union() {
        let input = Span::new("|");
        let result = parse_union(input);
        assert!(result.is_ok());
        let (rest, token) = result.unwrap();
        assert!(rest.fragment().is_empty());
        match token.token_type {
            TokenType::Union => (),
            _ => panic!("Expected Union token"),
        }
    }

    #[test]
    fn test_parse_nullable() {
        let input = Span::new("!");
        let result = parse_nullable(input);
        assert!(result.is_ok());
        let (rest, token) = result.unwrap();
        assert!(rest.fragment().is_empty());
        match token.token_type {
            TokenType::Nullable => (),
            _ => panic!("Expected Nullable token"),
        }
    }

    #[test]
    fn test_parse_array_start() {
        let input = Span::new("[");
        let result = parse_array_start(input);
        assert!(result.is_ok());
        let (rest, token) = result.unwrap();
        assert!(rest.fragment().is_empty());
        match token.token_type {
            TokenType::ArrayStart => (),
            _ => panic!("Expected ArrayStart token"),
        }
    }

    #[test]
    fn test_parse_array_end() {
        let input = Span::new("]");
        let result = parse_array_end(input);
        assert!(result.is_ok());
        let (rest, token) = result.unwrap();
        assert!(rest.fragment().is_empty());
        match token.token_type {
            TokenType::ArrayEnd => (),
            _ => panic!("Expected ArrayEnd token"),
        }
    }

    #[test]
    fn test_parse_curly_start() {
        let input = Span::new("{");
        let result = parse_curly_start(input);
        assert!(result.is_ok());
        let (rest, token) = result.unwrap();
        assert!(rest.fragment().is_empty());
        match token.token_type {
            TokenType::CurlyStart => (),
            _ => panic!("Expected CurlyStart token"),
        }
    }

    #[test]
    fn test_parse_curly_end() {
        let input = Span::new("}");
        let result = parse_curly_end(input);
        assert!(result.is_ok());
        let (rest, token) = result.unwrap();
        assert!(rest.fragment().is_empty());
        match token.token_type {
            TokenType::CurlyEnd => (),
            _ => panic!("Expected CurlyEnd token"),
        }
    }

    #[test]
    fn test_parse_bracket_start() {
        let input = Span::new("(");
        let result = parse_bracket_start(input);
        assert!(result.is_ok());
        let (rest, token) = result.unwrap();
        assert!(rest.fragment().is_empty());
        match token.token_type {
            TokenType::BracketStart => (),
            _ => panic!("Expected BracketStart token"),
        }
    }

    #[test]
    fn test_parse_bracket_end() {
        let input = Span::new(")");
        let result = parse_bracket_end(input);
        assert!(result.is_ok());
        let (rest, token) = result.unwrap();
        assert!(rest.fragment().is_empty());
        match token.token_type {
            TokenType::BracketEnd => (),
            _ => panic!("Expected BracketEnd token"),
        }
    }

    #[test]
    fn test_parse_semi_colon() {
        let input = Span::new(";");
        let result = parse_semi_colon(input);
        assert!(result.is_ok());
        let (rest, token) = result.unwrap();
        assert!(rest.fragment().is_empty());
        match token.token_type {
            TokenType::SemiColon => (),
            _ => panic!("Expected SemiColon token"),
        }
    }

    #[test]
    fn test_parse_identifier() {
        let input = Span::new("identifier");
        let result = parse_identifier(input);
        assert!(result.is_ok());
        let (rest, token) = result.unwrap();
        assert!(rest.fragment().is_empty());
        match token.token_type {
            TokenType::Identifier(ref s) if s == "identifier" => (),
            _ => panic!("Expected Identifier token"),
        }
    }

    #[test]
    fn test_parse_string_literal() {
        let input = Span::new("\"string\"");
        let result = parse_string_literal(input);
        assert!(result.is_ok());
        let (rest, token) = result.unwrap();
        assert!(rest.fragment().is_empty());
        match token.token_type {
            TokenType::StringLiteral(ref s) if s == "string" => (),
            _ => panic!("Expected StringLiteral token"),
        }
    }

    #[test]
    fn test_parse_number_literal() {
        let input = Span::new("123");
        let result = parse_number_literal(input);
        assert!(result.is_ok());
        let (rest, token) = result.unwrap();
        assert!(rest.fragment().is_empty());
        match token.token_type {
            TokenType::NumberLiteral(ref s) if s == "123" => (),
            _ => panic!("Expected NumberLiteral token"),
        }
    }

    #[test]
    fn test_parse_bool_literal() {
        let input = Span::new("true");
        let result = parse_bool_literal(input);
        assert!(result.is_ok());
        let (rest, token) = result.unwrap();
        assert!(rest.fragment().is_empty());
        match token.token_type {
            TokenType::BoolLiteral(ref s) if s == "true" => (),
            _ => panic!("Expected BoolLiteral token"),
        }

        let input_false = Span::new("false");
        let result_false = parse_bool_literal(input_false);
        assert!(result_false.is_ok());
        let (rest_false, token_false) = result_false.unwrap();
        assert!(rest_false.fragment().is_empty());
        match token_false.token_type {
            TokenType::BoolLiteral(ref s) if s == "false" => (),
            _ => panic!("Expected BoolLiteral token"),
        }
    }

    #[test]
    fn test_parse_token() {
        let input = Span::new("schema identifier: string;");
        let result = parse_token(input);
        assert!(result.is_ok());
        let (rest, token) = result.unwrap();
        if let Some(token) = token {
            match token.token_type {
                TokenType::Identifier(ref s) if s == "schema" => (),
                _ => panic!("Expected Identifier 'schema' token"),
            }
        } else {
            panic!("Expected token");
        }
        assert_eq!(*rest.fragment(), " identifier: string;");
    }

    #[test]
    fn test_lex() {
        let input = "schema identifier: string;";
        let result = lex(input).unwrap();
        let result = result
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<_>>();
        assert_eq!(
            result,
            vec![
                TokenType::Identifier("schema".to_string()),
                TokenType::Identifier("identifier".to_string()),
                TokenType::Colon,
                TokenType::Identifier("string".to_string()),
                TokenType::SemiColon,
            ]
        );
    }

    #[test]
    fn test_lex_error() {
        let input = "schema identifier: %";
        let result = lex(input);
        assert!(result.is_err());
        let binding = result.unwrap_err();
        let first_error = binding.first().unwrap();

        #[allow(unreachable_patterns)]
        match first_error {
            LexicalError::InvalidToken {
                line,
                column,
                message,
            } => {
                assert_eq!(*line, 1);
                assert_eq!(*column, 20); // Position of '%'
                assert_eq!(message, "%");
            }
            _ => {
                panic!("Expected LexicalError::InvalidToken");
            }
        }
    }

    #[test]
    fn test_2_lex_errors() {
        let input = r#"
%

%
        schema identifier: %"#;
        let result = lex(input);
        assert!(result.is_err());
        let binding = result.unwrap_err();
        let first_error = binding.first().unwrap();
        let second_error = binding.get(1).unwrap();
        let third_error = binding.get(2).unwrap();

        #[allow(unreachable_patterns)]
        match first_error {
            LexicalError::InvalidToken {
                line,
                column,
                message,
            } => {
                assert_eq!(*line, 2);
                assert_eq!(*column, 1);
                assert_eq!(message, "%");
            }
            _ => {
                panic!("Expected LexicalError::InvalidToken");
            }
        }

        #[allow(unreachable_patterns)]
        match second_error {
            LexicalError::InvalidToken {
                line,
                column,
                message,
            } => {
                assert_eq!(*line, 4);
                assert_eq!(*column, 1);
                assert_eq!(message, "%");
            }
            _ => {
                panic!("Expected LexicalError::InvalidToken");
            }
        }

        #[allow(unreachable_patterns)]
        match third_error {
            LexicalError::InvalidToken {
                line,
                column,
                message,
            } => {
                assert_eq!(*line, 5);
                assert_eq!(*column, 28);
                assert_eq!(message, "%");
            }
            _ => {
                panic!("Expected LexicalError::InvalidToken");
            }
        }
    }

    #[test]
    fn test_complex_lex() {
        let input = r#"
            schema HelloWorld {
                hello: string;
                world?: string;
            }
        "#;
        let result = lex(input).unwrap();
        let result = result
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<_>>();
        assert_eq!(
            result,
            vec![
                TokenType::Identifier("schema".to_string()),
                TokenType::Identifier("HelloWorld".to_string()),
                TokenType::CurlyStart,
                TokenType::Identifier("hello".to_string()),
                TokenType::Colon,
                TokenType::Identifier("string".to_string()),
                TokenType::SemiColon,
                TokenType::Identifier("world".to_string()),
                TokenType::Optional,
                TokenType::Colon,
                TokenType::Identifier("string".to_string()),
                TokenType::SemiColon,
                TokenType::CurlyEnd,
            ]
        );
    }

    #[test]
    fn test_can_lex() {
        let input = include_str!("../../data/schema/test.jus");
        let result = lex(input);
        assert!(result.is_ok());
    }
}

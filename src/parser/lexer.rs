use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, multispace0},
    IResult,
};

#[derive(Debug, thiserror::Error)]
pub enum LexicalError {
    #[error("Invalid token: {0}")]
    InvalidToken(String),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Token {
    Identifier(String),
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

fn parse_colon(input: &str) -> IResult<&str, Token> {
    let (input, _) = tag(":")(input)?;
    Ok((input, Token::Colon))
}

fn parse_optional(input: &str) -> IResult<&str, Token> {
    let (input, _) = tag("?")(input)?;
    Ok((input, Token::Optional))
}

fn parse_union(input: &str) -> IResult<&str, Token> {
    let (input, _) = tag("|")(input)?;
    Ok((input, Token::Union))
}

fn parse_nullable(input: &str) -> IResult<&str, Token> {
    let (input, _) = tag("!")(input)?;
    Ok((input, Token::Nullable))
}

fn parse_array_start(input: &str) -> IResult<&str, Token> {
    let (input, _) = tag("[")(input)?;
    Ok((input, Token::ArrayStart))
}

fn parse_array_end(input: &str) -> IResult<&str, Token> {
    let (input, _) = tag("]")(input)?;
    Ok((input, Token::ArrayEnd))
}

fn parse_curly_start(input: &str) -> IResult<&str, Token> {
    let (input, _) = tag("{")(input)?;
    Ok((input, Token::CurlyStart))
}

fn parse_curly_end(input: &str) -> IResult<&str, Token> {
    let (input, _) = tag("}")(input)?;
    Ok((input, Token::CurlyEnd))
}

fn parse_bracket_start(input: &str) -> IResult<&str, Token> {
    let (input, _) = tag("(")(input)?;
    Ok((input, Token::BracketStart))
}

fn parse_bracket_end(input: &str) -> IResult<&str, Token> {
    let (input, _) = tag(")")(input)?;
    Ok((input, Token::BracketEnd))
}

fn parse_semi_colon(input: &str) -> IResult<&str, Token> {
    let (input, _) = tag(";")(input)?;
    Ok((input, Token::SemiColon))
}

fn parse_identifier(input: &str) -> IResult<&str, Token> {
    let (input, identifier) = alpha1(input)?;
    Ok((input, Token::Identifier(identifier.to_string())))
}

fn skip_whitespace(input: &str) -> IResult<&str, &str> {
    let (input, _) = multispace0(input)?;
    Ok((input, input))
}

fn skip_newline(input: &str) -> IResult<&str, &str> {
    let (input, _) = tag("\n")(input)?;
    Ok((input, input))
}

fn parse_token(input: &str) -> IResult<&str, Option<Token>> {
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
        parse_identifier,
    ))(input)?;
    Ok((input, Some(token)))
}

pub(crate) fn lex(input: &str) -> Result<Vec<Token>, LexicalError> {
    let mut tokens = Vec::new();
    let mut input = input;
    while !input.is_empty() {
        let (rest, token) =
            parse_token(input).map_err(|e| LexicalError::InvalidToken(e.to_string()))?;
        match token {
            None => break,
            Some(token) => {
                tokens.push(token);
            }
        }
        input = rest;
    }
    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_colon() {
        let input = ":";
        let result = parse_colon(input);
        assert_eq!(result, Ok(("", Token::Colon)));
    }

    #[test]
    fn test_parse_optional() {
        let input = "?";
        let result = parse_optional(input);
        assert_eq!(result, Ok(("", Token::Optional)));
    }

    #[test]
    fn test_parse_union() {
        let input = "|";
        let result = parse_union(input);
        assert_eq!(result, Ok(("", Token::Union)));
    }

    #[test]
    fn test_parse_nullable() {
        let input = "!";
        let result = parse_nullable(input);
        assert_eq!(result, Ok(("", Token::Nullable)));
    }

    #[test]
    fn test_parse_array_start() {
        let input = "[";
        let result = parse_array_start(input);
        assert_eq!(result, Ok(("", Token::ArrayStart)));
    }

    #[test]
    fn test_parse_array_end() {
        let input = "]";
        let result = parse_array_end(input);
        assert_eq!(result, Ok(("", Token::ArrayEnd)));
    }

    #[test]
    fn test_parse_curly_start() {
        let input = "{";
        let result = parse_curly_start(input);
        assert_eq!(result, Ok(("", Token::CurlyStart)));
    }

    #[test]
    fn test_parse_curly_end() {
        let input = "}";
        let result = parse_curly_end(input);
        assert_eq!(result, Ok(("", Token::CurlyEnd)));
    }

    #[test]
    fn test_parse_bracket_start() {
        let input = "(";
        let result = parse_bracket_start(input);
        assert_eq!(result, Ok(("", Token::BracketStart)));
    }

    #[test]
    fn test_parse_bracket_end() {
        let input = ")";
        let result = parse_bracket_end(input);
        assert_eq!(result, Ok(("", Token::BracketEnd)));
    }

    #[test]
    fn test_parse_semi_colon() {
        let input = ";";
        let result = parse_semi_colon(input);
        assert_eq!(result, Ok(("", Token::SemiColon)));
    }

    #[test]
    fn test_parse_identifier() {
        let input = "identifier";
        let result = parse_identifier(input);
        assert_eq!(
            result,
            Ok(("", Token::Identifier("identifier".to_string())))
        );
    }

    #[test]
    fn test_parse_token() {
        let input = "schema identifier: string;";
        let result = parse_token(input);
        assert_eq!(
            result,
            Ok((
                " identifier: string;",
                Some(Token::Identifier("schema".to_string()))
            ))
        );
    }

    #[test]
    fn test_lex() {
        let input = "schema identifier: string;";
        let result = lex(input).unwrap();
        assert_eq!(
            result,
            vec![
                Token::Identifier("schema".to_string()),
                Token::Identifier("identifier".to_string()),
                Token::Colon,
                Token::Identifier("string".to_string()),
                Token::SemiColon,
            ]
        );
    }

    #[test]
    fn test_lex_error() {
        let input = "schema identifier: %";
        let result = lex(input);
        assert!(result.is_err());
    }
}

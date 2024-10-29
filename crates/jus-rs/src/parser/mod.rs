use self::lexer::{Token, TokenType};

pub mod lexer;

#[derive(Debug, Clone, thiserror::Error, PartialEq, Eq)]
pub enum ParseErrorKind {
    #[error("Lexical error: {0}")]
    LexicalError(#[from] lexer::LexicalError),
    #[error("Unexpected end of input")]
    UnexpectedEOF,
    #[error("Unexpected token: expected {expected:?}, found {found:?}")]
    UnexpectedToken {
        expected: lexer::TokenType,
        found: lexer::TokenType,
    },
    #[error("Expected identifier, found {0:?}")]
    ExpectedIdentifier(lexer::TokenType),
    #[error("Expected keyword '{0}', found {1:?}")]
    ExpectedKeyword(String, lexer::TokenType),
    #[error("Unknown type '{0}'")]
    UnknownType(String),
    #[error("Expected type, found {0:?}")]
    ExpectedType(lexer::TokenType),
    #[error("Multiple schema definitions found: {second:?} but already have {first:?}")]
    MultipleSchemaDefinitions { first: String, second: String },
    #[error("No schema found")]
    NoSchemaFound,
}

#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub line: usize,
    pub column: usize,
    pub width: usize,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "Parse error: {} at line {} column {} width {}",
            self.kind, self.line, self.column, self.width
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveType {
    String,
    Number,
    Boolean,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralType {
    String(String),
    Number(String),
    Boolean(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeExpr {
    Primitive(PrimitiveType),
    Literal(LiteralType),
    Object(Vec<Field>),
    Array(Box<TypeExpr>),
    AliasReference(String),
    Nullable(Box<TypeExpr>),
    Union(Box<TypeExpr>, Box<TypeExpr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: String,
    pub type_expr: TypeExpr,
    pub is_optional: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAlias {
    pub name: String,
    pub type_expr: TypeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Schema {
    pub name: String,
    pub fields: Vec<Field>,
}

#[derive(Debug, PartialEq)]
pub struct Ast {
    pub schema: Schema,
    pub type_aliases: Vec<TypeAlias>,
}

pub(crate) struct Parser<'a> {
    tokens: Vec<lexer::Token<'a>>,
    index: usize,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(tokens: Vec<lexer::Token<'a>>) -> Self {
        Self { tokens, index: 0 }
    }

    pub fn expect_token(&mut self, expected: lexer::TokenType) -> Result<(), ParseError> {
        if let Some(token) = self.current_token() {
            if token.token_type == expected {
                self.advance();
                Ok(())
            } else {
                Err(ParseError {
                    kind: ParseErrorKind::UnexpectedToken {
                        expected,
                        found: token.token_type.clone(),
                    },
                    line: token.position.location_line() as usize,
                    column: token.position.get_column(),
                    width: token.width(),
                })
            }
        } else {
            Err(ParseError {
                kind: ParseErrorKind::UnexpectedEOF,
                line: 1,
                column: 1,
                width: 1,
            })
        }
    }

    pub fn advance(&mut self) {
        self.index += 1;
    }

    pub fn current_token(&self) -> Option<lexer::Token<'a>> {
        self.tokens.get(self.index).cloned()
    }

    pub fn expect_keyword(&mut self, keyword: &str) -> Result<(), ParseError> {
        if let Some(token) = self.current_token() {
            if let lexer::TokenType::Identifier(ident) = &token.token_type {
                match ident.as_str() {
                    word if word == keyword => {
                        self.advance();
                        Ok(())
                    }
                    _ => Err(ParseError {
                        kind: ParseErrorKind::ExpectedKeyword(
                            keyword.to_string(),
                            token.token_type.clone(),
                        ),
                        line: token.position.location_line() as usize,
                        column: token.position.get_column(),
                        width: token.width(),
                    }),
                }
            } else {
                Err(ParseError {
                    kind: ParseErrorKind::ExpectedKeyword(
                        keyword.to_string(),
                        token.token_type.clone(),
                    ),
                    line: token.position.location_line() as usize,
                    column: token.position.get_column(),
                    width: token.width(),
                })
            }
        } else {
            Err(ParseError {
                kind: ParseErrorKind::UnexpectedEOF,
                line: 1,
                column: 1,
                width: 1,
            })
        }
    }

    pub fn try_parse_union_right(&mut self) -> Option<TypeExpr> {
        if let Some(Token {
            token_type: TokenType::Union,
            ..
        }) = self.current_token()
        {
            self.advance();
            self.parse_type_expression().ok()
        } else {
            None
        }
    }

    pub fn parse_type_expression(&mut self) -> Result<TypeExpr, ParseError> {
        if let Some(token) = self.current_token() {
            let left = match token.token_type {
                lexer::TokenType::Identifier(ident) => {
                    let ident = ident.clone();
                    let type_expr = match ident.as_str() {
                        "string" => TypeExpr::Primitive(PrimitiveType::String),
                        "number" => TypeExpr::Primitive(PrimitiveType::Number),
                        "boolean" => TypeExpr::Primitive(PrimitiveType::Boolean),
                        _ => TypeExpr::AliasReference(ident),
                    };
                    self.advance();
                    Ok(type_expr)
                }
                lexer::TokenType::Nullable => {
                    self.advance();
                    let inner = self.parse_type_expression()?;
                    Ok(TypeExpr::Nullable(Box::new(inner)))
                }
                lexer::TokenType::CurlyStart => {
                    self.advance();
                    let mut fields = Vec::new();
                    while let Some(Token {
                        token_type: lexer::TokenType::Identifier(_),
                        ..
                    }) = self.current_token()
                    {
                        let field = self.parse_field()?;
                        fields.push(field);
                    }
                    self.expect_token(lexer::TokenType::CurlyEnd)?;
                    Ok(TypeExpr::Object(fields))
                }
                lexer::TokenType::ArrayStart => {
                    self.advance();
                    let inner = self.parse_type_expression()?;
                    self.expect_token(lexer::TokenType::ArrayEnd)?;
                    Ok(TypeExpr::Array(Box::new(inner)))
                }
                lexer::TokenType::StringLiteral(s) => {
                    let s = s.clone();
                    self.advance();
                    Ok(TypeExpr::Literal(LiteralType::String(s)))
                }
                lexer::TokenType::NumberLiteral(n) => {
                    let n = n.clone();
                    self.advance();
                    Ok(TypeExpr::Literal(LiteralType::Number(n)))
                }
                lexer::TokenType::BoolLiteral(b) => {
                    let b = b.clone();
                    self.advance();
                    Ok(TypeExpr::Literal(LiteralType::Boolean(b == "true")))
                }
                _ => Err(ParseError {
                    kind: ParseErrorKind::ExpectedType(token.token_type.clone()),
                    line: token.position.location_line() as usize,
                    column: token.position.get_column(),
                    width: token.width(),
                }),
            }?;
            if let Some(right) = self.try_parse_union_right() {
                Ok(TypeExpr::Union(Box::new(left), Box::new(right)))
            } else {
                Ok(left)
            }
        } else {
            Err(ParseError {
                kind: ParseErrorKind::UnexpectedEOF,
                line: 1,
                column: 1,
                width: 1,
            })
        }
    }

    pub fn parse_identifier(&mut self) -> Result<String, ParseError> {
        if let Some(token) = self.current_token() {
            if let lexer::TokenType::Identifier(ident) = token.token_type {
                let ident = ident.clone();
                self.advance();
                Ok(ident)
            } else {
                Err(ParseError {
                    kind: ParseErrorKind::ExpectedIdentifier(token.token_type.clone()),
                    line: token.position.location_line() as usize,
                    column: token.position.get_column(),
                    width: token.width(),
                })
            }
        } else {
            Err(ParseError {
                kind: ParseErrorKind::UnexpectedEOF,
                line: 1,
                column: 1,
                width: 1,
            })
        }
    }

    pub fn parse_field(&mut self) -> Result<Field, ParseError> {
        let field_name = self.parse_identifier()?;

        let is_optional = if let Some(Token {
            token_type: TokenType::Optional,
            ..
        }) = self.current_token()
        {
            self.advance();
            true
        } else {
            false
        };

        self.expect_token(lexer::TokenType::Colon)?;

        let type_expr = self.parse_type_expression()?;

        self.expect_token(lexer::TokenType::SemiColon)?;

        Ok(Field {
            name: field_name,
            type_expr,
            is_optional,
        })
    }

    pub fn parse_type_alias(&mut self) -> Result<TypeAlias, ParseError> {
        self.expect_keyword("type")?;
        let alias_name = self.parse_identifier()?;
        let type_expr = self.parse_type_expression()?;
        self.expect_token(lexer::TokenType::SemiColon)?;
        Ok(TypeAlias {
            name: alias_name,
            type_expr,
        })
    }

    pub fn parse_schema(&mut self) -> Result<Schema, ParseError> {
        self.expect_keyword("schema")?;
        let schema_name = self.parse_identifier()?;
        self.expect_token(lexer::TokenType::CurlyStart)?;

        let mut fields = Vec::new();
        while let Some(Token {
            token_type: lexer::TokenType::Identifier(_),
            ..
        }) = self.current_token()
        {
            let field = self.parse_field()?;
            fields.push(field);
        }
        self.expect_token(lexer::TokenType::CurlyEnd)?;
        Ok(Schema {
            name: schema_name,
            fields,
        })
    }

    pub fn parse(&mut self) -> Result<Ast, Vec<ParseError>> {
        let mut schema: Option<Schema> = None;
        let mut errors = Vec::new();
        let mut type_aliases = Vec::new();

        while let Some(token) = self.current_token() {
            match token.token_type {
                lexer::TokenType::Identifier(ref keyword) => match keyword.as_str() {
                    "schema" => {
                        match self.parse_schema() {
                            Ok(parsed_schema) => {
                                if let Some(ref existing_schema) = schema {
                                    errors.push(ParseError {
                                        kind: ParseErrorKind::MultipleSchemaDefinitions {
                                            first: existing_schema.name.clone(),
                                            second: parsed_schema.name,
                                        },
                                        line: token.position.location_line() as usize,
                                        column: token.position.get_column(),
                                        width: token.width(),
                                    });
                                } else {
                                    schema = Some(parsed_schema);
                                }
                            }
                            Err(e) => {
                                errors.push(e);
                                self.advance();
                            }
                        };
                    }
                    "type" => match self.parse_type_alias() {
                        Ok(type_alias) => type_aliases.push(type_alias),
                        Err(e) => {
                            errors.push(e);
                            self.advance();
                        }
                    },
                    _ => {
                        errors.push(ParseError {
                            kind: ParseErrorKind::UnknownType(keyword.clone()),
                            line: token.position.location_line() as usize,
                            column: token.position.get_column(),
                            width: token.width(),
                        });
                        self.advance();
                    }
                },
                _ => {
                    self.advance();
                }
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        if schema.is_none() {
            errors.push(ParseError {
                kind: ParseErrorKind::NoSchemaFound,
                line: 1,
                column: 1,
                width: 1,
            });
        }

        if errors.is_empty() {
            Ok(Ast {
                schema: schema.unwrap(),
                type_aliases,
            })
        } else {
            Err(errors)
        }
    }
}

pub fn parse_ast(input: &str) -> Result<Ast, Vec<ParseError>> {
    match lexer::lex(input) {
        Ok(tokens) => {
            let mut parser = Parser::new(tokens);
            parser.parse()
        }
        Err(e) => Err(e
            .into_iter()
            .map(|e| {
                let lexer::LexicalError::InvalidToken { column, line, .. } = e;
                ParseError {
                    kind: ParseErrorKind::LexicalError(e),
                    line: line as usize,
                    column,
                    width: 1,
                }
            })
            .collect()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_primitive() {
        let input = "string";
        let tokens = lexer::lex(input).unwrap();
        let mut parser = Parser::new(tokens);
        let result = parser.parse_type_expression().unwrap();
        assert_eq!(result, TypeExpr::Primitive(PrimitiveType::String));
    }

    #[test]
    fn test_parse_nullable() {
        let input = "!string";
        let tokens = lexer::lex(input).unwrap();
        let mut parser = Parser::new(tokens);
        let result = parser.parse_type_expression().unwrap();
        assert_eq!(
            result,
            TypeExpr::Nullable(Box::new(TypeExpr::Primitive(PrimitiveType::String)))
        );
    }

    #[test]
    fn test_parse_union() {
        let input = "string | number";
        let tokens = lexer::lex(input).unwrap();
        let mut parser = Parser::new(tokens);
        let result = parser.parse_type_expression().unwrap();
        assert_eq!(
            result,
            TypeExpr::Union(
                Box::new(TypeExpr::Primitive(PrimitiveType::String)),
                Box::new(TypeExpr::Primitive(PrimitiveType::Number))
            )
        );
    }

    #[test]
    fn test_parse_field() {
        let input = "name: string;";
        let tokens = lexer::lex(input).unwrap();
        let mut parser = Parser::new(tokens);
        let result = parser.parse_field().unwrap();
        assert_eq!(
            result,
            Field {
                name: "name".to_string(),
                type_expr: TypeExpr::Primitive(PrimitiveType::String),
                is_optional: false,
            }
        );
    }

    #[test]
    fn test_parse_optional_field() {
        let input = "name?: string;";
        let tokens = lexer::lex(input).unwrap();
        let mut parser = Parser::new(tokens);
        let result = parser.parse_field().unwrap();
        assert_eq!(
            result,
            Field {
                name: "name".to_string(),
                type_expr: TypeExpr::Primitive(PrimitiveType::String),
                is_optional: true,
            }
        );
    }

    #[test]
    fn test_parse_schema() {
        let input = "schema Person { name: string; age: number; }";
        let tokens = lexer::lex(input).unwrap();
        let mut parser = Parser::new(tokens);
        let result = parser.parse_schema().unwrap();
        assert_eq!(
            result,
            Schema {
                name: "Person".to_string(),
                fields: vec![
                    Field {
                        name: "name".to_string(),
                        type_expr: TypeExpr::Primitive(PrimitiveType::String),
                        is_optional: false,
                    },
                    Field {
                        name: "age".to_string(),
                        type_expr: TypeExpr::Primitive(PrimitiveType::Number),
                        is_optional: false,
                    }
                ]
            }
        );
    }

    #[test]
    fn test_parse_type_alias() {
        let input = "type Name string;";
        let tokens = lexer::lex(input).unwrap();
        let mut parser = Parser::new(tokens);
        let result = parser.parse_type_alias().unwrap();
        assert_eq!(
            result,
            TypeAlias {
                name: "Name".to_string(),
                type_expr: TypeExpr::Primitive(PrimitiveType::String),
            }
        );
    }

    #[test]
    fn test_parse() {
        let input = r#"
            type Name string;
            schema Person { name: Name; age: number; }
        "#;
        let result = parse_ast(input).unwrap();
        assert_eq!(
            result,
            Ast {
                schema: Schema {
                    name: "Person".to_string(),
                    fields: vec![
                        Field {
                            name: "name".to_string(),
                            type_expr: TypeExpr::AliasReference("Name".to_string()),
                            is_optional: false,
                        },
                        Field {
                            name: "age".to_string(),
                            type_expr: TypeExpr::Primitive(PrimitiveType::Number),
                            is_optional: false,
                        }
                    ]
                },
                type_aliases: vec![TypeAlias {
                    name: "Name".to_string(),
                    type_expr: TypeExpr::Primitive(PrimitiveType::String),
                }]
            }
        );
    }

    #[test]
    fn test_parse_simple_sample() {
        let input = include_str!("../../data/schema/simple.jus");
        let result = parse_ast(input).unwrap();
        assert_eq!(
            result,
            Ast {
                schema: Schema {
                    name: "HelloWorld".to_string(),
                    fields: vec![
                        Field {
                            name: "hello".to_string(),
                            type_expr: TypeExpr::Primitive(PrimitiveType::String),
                            is_optional: false,
                        },
                        Field {
                            name: "world".to_string(),
                            type_expr: TypeExpr::Primitive(PrimitiveType::String),
                            is_optional: true,
                        }
                    ]
                },
                type_aliases: vec![]
            }
        );
    }

    #[test]
    fn test_parse_complex_sample() {
        let input = include_str!("../../data/schema/address.jus");
        let result = parse_ast(input).unwrap();
        assert_eq!(
            result,
            Ast {
                schema: Schema {
                    name: "Address".to_string(),
                    fields: vec![
                        Field {
                            name: "postOfficeBox".to_string(),
                            type_expr: TypeExpr::Primitive(PrimitiveType::String),
                            is_optional: true,
                        },
                        Field {
                            name: "extendedAddress".to_string(),
                            type_expr: TypeExpr::Primitive(PrimitiveType::String),
                            is_optional: true,
                        },
                        Field {
                            name: "streetAddress".to_string(),
                            type_expr: TypeExpr::Primitive(PrimitiveType::String),
                            is_optional: true,
                        },
                        Field {
                            name: "locality".to_string(),
                            type_expr: TypeExpr::Nullable(Box::new(TypeExpr::Primitive(
                                PrimitiveType::String
                            ))),
                            is_optional: false,
                        },
                        Field {
                            name: "region".to_string(),
                            type_expr: TypeExpr::Primitive(PrimitiveType::String),
                            is_optional: false,
                        },
                        Field {
                            name: "postalOrZipCode".to_string(),
                            type_expr: TypeExpr::Union(
                                Box::new(TypeExpr::AliasReference("PostalCode".to_string())),
                                Box::new(TypeExpr::AliasReference("ZipCode".to_string()))
                            ),
                            is_optional: false,
                        },
                        Field {
                            name: "countryName".to_string(),
                            type_expr: TypeExpr::Primitive(PrimitiveType::String),
                            is_optional: false,
                        }
                    ]
                },
                type_aliases: vec![
                    TypeAlias {
                        name: "PostalCode".to_string(),
                        type_expr: TypeExpr::Primitive(PrimitiveType::String),
                    },
                    TypeAlias {
                        name: "ZipCode".to_string(),
                        type_expr: TypeExpr::Primitive(PrimitiveType::Number),
                    }
                ]
            }
        );
    }

    #[test]
    fn test_parse_nested_sample() {
        let input = include_str!("../../data/schema/nested.jus");
        let result = parse_ast(input).unwrap();
        assert_eq!(
            result,
            Ast {
                schema: Schema {
                    name: "Nested".to_string(),
                    fields: vec![
                        Field {
                            name: "obj".to_string(),
                            type_expr: TypeExpr::Object(vec![Field {
                                name: "hello".to_string(),
                                type_expr: TypeExpr::Primitive(PrimitiveType::String),
                                is_optional: false,
                            }]),
                            is_optional: false,
                        },
                        Field {
                            name: "arr".to_string(),
                            type_expr: TypeExpr::Array(Box::new(TypeExpr::Primitive(
                                PrimitiveType::Number
                            ))),
                            is_optional: false,
                        }
                    ],
                },
                type_aliases: vec![]
            }
        );
    }

    #[test]
    fn test_multiple_schemas_found_error() {
        let input = r#"
            schema Person { name: string; age: number; }
            schema Details { address: string; }
        "#;
        let result = parse_ast(input);
        assert!(result.is_err());
        let result = result.unwrap_err();
        assert_eq!(result.len(), 1);
        let error = result[0].kind.clone();
        assert_eq!(
            error,
            ParseErrorKind::MultipleSchemaDefinitions {
                first: "Person".to_string(),
                second: "Details".to_string()
            }
        );
    }

    #[test]
    fn no_schema_found_error() {
        let input = r#"
            type Name string;
        "#;
        let result = parse_ast(input);
        assert!(result.is_err());
        let result = result.unwrap_err();
        assert_eq!(result.len(), 1);
        let error = result[0].kind.clone();
        assert_eq!(error, ParseErrorKind::NoSchemaFound);
    }

    #[test]
    fn parse_complex() {
        let input = include_str!("../../data/schema/complex.jus");
        let result = parse_ast(input).unwrap();
        assert_eq!(
            result,
            Ast {
                schema: Schema {
                    name: "UserProfile".to_string(),
                    fields: vec![
                        Field {
                            name: "id".to_string(),
                            type_expr: TypeExpr::AliasReference("UserID".to_string()),
                            is_optional: false,
                        },
                        Field {
                            name: "name".to_string(),
                            type_expr: TypeExpr::Primitive(PrimitiveType::String),
                            is_optional: false,
                        },
                        Field {
                            name: "username".to_string(),
                            type_expr: TypeExpr::Primitive(PrimitiveType::String),
                            is_optional: false,
                        },
                        Field {
                            name: "contactInfo".to_string(),
                            type_expr: TypeExpr::AliasReference("ContactInfo".to_string()),
                            is_optional: false,
                        },
                        Field {
                            name: "friends".to_string(),
                            type_expr: TypeExpr::Array(Box::new(TypeExpr::AliasReference(
                                "UserID".to_string()
                            ))),
                            is_optional: true,
                        },
                        Field {
                            name: "status".to_string(),
                            type_expr: TypeExpr::Union(
                                Box::new(TypeExpr::Literal(LiteralType::String(
                                    "active".to_string()
                                ))),
                                Box::new(TypeExpr::Union(
                                    Box::new(TypeExpr::Literal(LiteralType::String(
                                        "inactive".to_string()
                                    ))),
                                    Box::new(TypeExpr::Literal(LiteralType::String(
                                        "banned".to_string()
                                    )))
                                ))
                            ),
                            is_optional: true,
                        },
                        Field {
                            name: "bio".to_string(),
                            type_expr: TypeExpr::Nullable(Box::new(TypeExpr::Primitive(
                                PrimitiveType::String
                            ))),
                            is_optional: true,
                        },
                        Field {
                            name: "preferences".to_string(),
                            type_expr: TypeExpr::Object(vec![
                                Field {
                                    name: "theme".to_string(),
                                    type_expr: TypeExpr::Union(
                                        Box::new(TypeExpr::Literal(LiteralType::String(
                                            "light".to_string()
                                        ))),
                                        Box::new(TypeExpr::Union(
                                            Box::new(TypeExpr::Literal(LiteralType::String(
                                                "dark".to_string()
                                            ))),
                                            Box::new(TypeExpr::Literal(LiteralType::String(
                                                "system".to_string()
                                            )))
                                        ))
                                    ),
                                    is_optional: false,
                                },
                                Field {
                                    name: "notifications".to_string(),
                                    type_expr: TypeExpr::Object(vec![
                                        Field {
                                            name: "email".to_string(),
                                            type_expr: TypeExpr::Primitive(PrimitiveType::Boolean),
                                            is_optional: false,
                                        },
                                        Field {
                                            name: "sms".to_string(),
                                            type_expr: TypeExpr::Primitive(PrimitiveType::Boolean),
                                            is_optional: false,
                                        },
                                        Field {
                                            name: "push".to_string(),
                                            type_expr: TypeExpr::Primitive(PrimitiveType::Boolean),
                                            is_optional: false,
                                        }
                                    ]),
                                    is_optional: false,
                                }
                            ]),
                            is_optional: true,
                        }
                    ]
                },
                type_aliases: vec![
                    TypeAlias {
                        name: "EmailAddress".to_string(),
                        type_expr: TypeExpr::Primitive(PrimitiveType::String),
                    },
                    TypeAlias {
                        name: "PhoneNumber".to_string(),
                        type_expr: TypeExpr::Primitive(PrimitiveType::String),
                    },
                    TypeAlias {
                        name: "SocialMediaHandle".to_string(),
                        type_expr: TypeExpr::Primitive(PrimitiveType::String),
                    },
                    TypeAlias {
                        name: "UserID".to_string(),
                        type_expr: TypeExpr::Primitive(PrimitiveType::String),
                    },
                    TypeAlias {
                        name: "ContactInfo".to_string(),
                        type_expr: TypeExpr::Object(vec![
                            Field {
                                name: "email".to_string(),
                                type_expr: TypeExpr::AliasReference("EmailAddress".to_string()),
                                is_optional: false,
                            },
                            Field {
                                name: "phone".to_string(),
                                type_expr: TypeExpr::AliasReference("PhoneNumber".to_string()),
                                is_optional: true,
                            },
                            Field {
                                name: "socialMedia".to_string(),
                                type_expr: TypeExpr::Object(vec![
                                    Field {
                                        name: "twitter".to_string(),
                                        type_expr: TypeExpr::AliasReference(
                                            "SocialMediaHandle".to_string()
                                        ),
                                        is_optional: true,
                                    },
                                    Field {
                                        name: "linkedin".to_string(),
                                        type_expr: TypeExpr::AliasReference(
                                            "SocialMediaHandle".to_string()
                                        ),
                                        is_optional: true,
                                    },
                                    Field {
                                        name: "github".to_string(),
                                        type_expr: TypeExpr::AliasReference(
                                            "SocialMediaHandle".to_string()
                                        ),
                                        is_optional: true,
                                    }
                                ]),
                                is_optional: true,
                            }
                        ])
                    },
                ]
            }
        );
    }
}

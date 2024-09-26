pub mod lexer;

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum ParseError {
    #[error("Lexical error: {0}")]
    LexicalError(#[from] lexer::LexicalError),
    #[error("Unexpected end of input")]
    UnexpectedEOF,
    #[error("Unexpected token: expected {expected:?}, found {found:?}")]
    UnexpectedToken {
        expected: lexer::Token,
        found: lexer::Token,
    },
    #[error("Expected identifier, found {0:?}")]
    ExpectedIdentifier(lexer::Token),
    #[error("Expected keyword '{0}', found {1:?}")]
    ExpectedKeyword(String, lexer::Token),
    #[error("Unknown type '{0}'")]
    UnknownType(String),
    #[error("Expected type, found {0:?}")]
    ExpectedType(lexer::Token),
    #[error("Multiple schema definitions found: {second:?} but already have {first:?}")]
    MultipleSchemaDefinitions { first: String, second: String },
    #[error("No schema found")]
    NoSchemaFound,
}

#[derive(Debug, PartialEq)]
pub enum PrimitiveType {
    String,
    Number,
    Boolean,
}

#[derive(Debug, PartialEq)]
pub enum LiteralType {
    String(String),
    Number(String),
    Boolean(bool),
}

#[derive(Debug, PartialEq)]
pub enum TypeExpr {
    Primitive(PrimitiveType),
    Literal(LiteralType),
    Object(Vec<Field>),
    Array(Box<TypeExpr>),
    AliasReference(String),
    Nullable(Box<TypeExpr>),
    Union(Box<TypeExpr>, Box<TypeExpr>),
}

#[derive(Debug, PartialEq)]
pub struct Field {
    pub name: String,
    pub type_expr: TypeExpr,
    pub is_optional: bool,
}

#[derive(Debug, PartialEq)]
pub struct TypeAlias {
    pub name: String,
    pub type_expr: TypeExpr,
}

#[derive(Debug, PartialEq)]
pub struct Schema {
    pub name: String,
    pub fields: Vec<Field>,
}

#[derive(Debug, PartialEq)]
pub struct Ast {
    pub schema: Schema,
    pub type_aliases: Vec<TypeAlias>,
}

pub(crate) struct Parser {
    tokens: Vec<lexer::Token>,
    index: usize,
}

impl Parser {
    pub(crate) fn new(tokens: Vec<lexer::Token>) -> Self {
        Self { tokens, index: 0 }
    }

    pub fn expect_token(&mut self, expected: lexer::Token) -> Result<(), ParseError> {
        if let Some(token) = self.current_token() {
            if token == &expected {
                self.advance();
                Ok(())
            } else {
                Err(ParseError::UnexpectedToken {
                    expected,
                    found: token.clone(),
                })
            }
        } else {
            Err(ParseError::UnexpectedEOF)
        }
    }

    pub fn advance(&mut self) {
        self.index += 1;
    }

    pub fn current_token(&self) -> Option<&lexer::Token> {
        self.tokens.get(self.index)
    }

    pub fn expect_keyword(&mut self, keyword: &str) -> Result<(), ParseError> {
        if let Some(token) = self.current_token() {
            if let lexer::Token::Identifier(ident) = token {
                match ident.as_str() {
                    word if word == keyword => {
                        self.advance();
                        Ok(())
                    }
                    _ => Err(ParseError::ExpectedKeyword(
                        keyword.to_string(),
                        token.clone(),
                    )),
                }
            } else {
                Err(ParseError::ExpectedKeyword(
                    keyword.to_string(),
                    token.clone(),
                ))
            }
        } else {
            Err(ParseError::UnexpectedEOF)
        }
    }

    pub fn try_parse_union_right(&mut self) -> Option<TypeExpr> {
        if let Some(lexer::Token::Union) = self.current_token() {
            self.advance();
            self.parse_type_expression().ok()
        } else {
            None
        }
    }

    pub fn parse_type_expression(&mut self) -> Result<TypeExpr, ParseError> {
        if let Some(token) = self.current_token() {
            let left = match token {
                lexer::Token::Identifier(ident) => {
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
                lexer::Token::Nullable => {
                    self.advance();
                    let inner = self.parse_type_expression()?;
                    Ok(TypeExpr::Nullable(Box::new(inner)))
                }
                lexer::Token::CurlyStart => {
                    self.advance();
                    let mut fields = Vec::new();
                    while let Some(lexer::Token::Identifier(_)) = self.current_token() {
                        let field = self.parse_field()?;
                        fields.push(field);
                    }
                    self.expect_token(lexer::Token::CurlyEnd)?;
                    Ok(TypeExpr::Object(fields))
                }
                lexer::Token::ArrayStart => {
                    self.advance();
                    let inner = self.parse_type_expression()?;
                    self.expect_token(lexer::Token::ArrayEnd)?;
                    Ok(TypeExpr::Array(Box::new(inner)))
                }
                lexer::Token::StringLiteral(s) => {
                    let s = s.clone();
                    self.advance();
                    Ok(TypeExpr::Literal(LiteralType::String(s)))
                }
                lexer::Token::NumberLiteral(n) => {
                    let n = n.clone();
                    self.advance();
                    Ok(TypeExpr::Literal(LiteralType::Number(n)))
                }
                lexer::Token::BoolLiteral(b) => {
                    let b = b.clone();
                    self.advance();
                    Ok(TypeExpr::Literal(LiteralType::Boolean(b == "true")))
                }
                _ => Err(ParseError::ExpectedType(token.clone())),
            }?;
            if let Some(right) = self.try_parse_union_right() {
                Ok(TypeExpr::Union(Box::new(left), Box::new(right)))
            } else {
                Ok(left)
            }
        } else {
            Err(ParseError::UnexpectedEOF)
        }
    }

    pub fn parse_identifier(&mut self) -> Result<String, ParseError> {
        if let Some(token) = self.current_token() {
            if let lexer::Token::Identifier(ident) = token {
                let ident = ident.clone();
                self.advance();
                Ok(ident)
            } else {
                Err(ParseError::ExpectedIdentifier(token.clone()))
            }
        } else {
            Err(ParseError::UnexpectedEOF)
        }
    }

    pub fn parse_field(&mut self) -> Result<Field, ParseError> {
        let field_name = self.parse_identifier()?;

        let is_optional = if let Some(lexer::Token::Optional) = self.current_token() {
            self.advance();
            true
        } else {
            false
        };

        self.expect_token(lexer::Token::Colon)?;

        let type_expr = self.parse_type_expression()?;

        self.expect_token(lexer::Token::SemiColon)?;

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
        self.expect_token(lexer::Token::SemiColon)?;
        Ok(TypeAlias {
            name: alias_name,
            type_expr,
        })
    }

    pub fn parse_schema(&mut self) -> Result<Schema, ParseError> {
        self.expect_keyword("schema")?;
        let schema_name = self.parse_identifier()?;
        self.expect_token(lexer::Token::CurlyStart)?;

        let mut fields = Vec::new();
        while let Some(lexer::Token::Identifier(_)) = self.current_token() {
            let field = self.parse_field()?;
            fields.push(field);
        }
        self.expect_token(lexer::Token::CurlyEnd)?;
        Ok(Schema {
            name: schema_name,
            fields,
        })
    }

    pub fn parse(&mut self) -> Result<Ast, ParseError> {
        let mut schema: Option<Schema> = None;
        let mut type_aliases = Vec::new();

        while let Some(token) = self.current_token() {
            match token {
                lexer::Token::Identifier(keyword) => match keyword.as_str() {
                    "schema" => {
                        let parsed_schema = self.parse_schema()?;
                        if let Some(existing_schema) = schema {
                            return Err(ParseError::MultipleSchemaDefinitions {
                                first: existing_schema.name,
                                second: parsed_schema.name,
                            });
                        } else {
                            schema = Some(parsed_schema);
                        }
                    }
                    "type" => {
                        let type_alias = self.parse_type_alias()?;
                        type_aliases.push(type_alias);
                    }
                    _ => return Err(ParseError::UnknownType(keyword.clone())),
                },
                _ => {
                    return Err(ParseError::UnexpectedToken {
                        expected: lexer::Token::Identifier("schema".to_string()),
                        found: token.clone(),
                    })
                }
            }
        }

        if let Some(schema) = schema {
            Ok(Ast {
                schema,
                type_aliases,
            })
        } else {
            Err(ParseError::NoSchemaFound)
        }
    }
}

pub(crate) fn parse_ast(input: &str) -> Result<Ast, ParseError> {
    let tokens = lexer::lex(input)?;
    let mut parser = Parser::new(tokens);
    parser.parse()
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
        let input = include_str!("../../samples/simple.jus");
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
        let input = include_str!("../../samples/address.jus");
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
        let input = include_str!("../../samples/nested.jus");
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
        assert_eq!(
            result.unwrap_err(),
            ParseError::MultipleSchemaDefinitions {
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
        assert_eq!(result.unwrap_err(), ParseError::NoSchemaFound);
    }

    #[test]
    fn parse_complex() {
        let input = include_str!("../../samples/complex.jus");
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

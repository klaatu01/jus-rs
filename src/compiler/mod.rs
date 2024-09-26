use std::collections::{BTreeMap, HashMap};

use crate::parser::{Ast, LiteralType, PrimitiveType, Schema, TypeAlias, TypeExpr};

use self::validator::{LiteralValidation, Pointer, TypeValidation, Validation, Validator};

pub(crate) mod validator;

pub(crate) fn compile_primitive_validation(primitive: &PrimitiveType) -> Validation {
    match primitive {
        PrimitiveType::String => Validation::Type(TypeValidation::String),
        PrimitiveType::Number => Validation::Type(TypeValidation::Number),
        PrimitiveType::Boolean => Validation::Type(TypeValidation::Boolean),
    }
}

pub(crate) fn compile_literal_validation(literal: &LiteralType) -> Validation {
    match literal {
        LiteralType::String(s) => Validation::Literal(LiteralValidation::String(s.clone())),
        LiteralType::Number(n) => Validation::Literal(LiteralValidation::Number(n.clone())),
        LiteralType::Boolean(b) => Validation::Literal(LiteralValidation::Boolean(*b)),
    }
}

pub(crate) fn compile_type_expr_into_validator(
    type_expr: &TypeExpr,
    pointer: &str,
    is_optional: bool,
    type_aliases: &HashMap<String, TypeExpr>,
) -> Validator {
    match type_expr {
        TypeExpr::Primitive(primitive) => {
            let validation = compile_primitive_validation(primitive);
            Validator::Primitive {
                pointer: Pointer(pointer.to_string()),
                validation,
                is_optional,
            }
        }
        TypeExpr::Literal(literal) => {
            let validation = compile_literal_validation(literal);
            Validator::Primitive {
                pointer: Pointer(pointer.to_string()),
                validation,
                is_optional,
            }
        }
        TypeExpr::Object(fields) => {
            let mut field_validators = Vec::new();
            for field in fields {
                let field_pointer = format!("/{}", field.name);
                let field_validator = compile_type_expr_into_validator(
                    &field.type_expr,
                    &field_pointer,
                    field.is_optional,
                    type_aliases,
                );
                field_validators.push(field_validator);
            }
            Validator::Object {
                pointer: Pointer(pointer.to_string()),
                field_validators,
                is_optional,
            }
        }
        TypeExpr::Array(element_type) => {
            let element_validator =
                compile_type_expr_into_validator(element_type, "", false, type_aliases);
            Validator::Array {
                pointer: Pointer(pointer.to_string()),
                element_validator: Box::new(element_validator),
                is_optional,
            }
        }
        TypeExpr::Nullable(inner) => {
            let inner_validator =
                compile_type_expr_into_validator(inner, pointer, false, type_aliases);
            Validator::Primitive {
                pointer: Pointer(pointer.to_string()),
                validation: Validation::Or(
                    Box::new(Validation::Null),
                    Box::new(inner_validator.get_validation()),
                ),
                is_optional,
            }
        }
        TypeExpr::Union(left, right) => {
            let left_validator =
                compile_type_expr_into_validator(left, pointer, false, type_aliases);
            let right_validator =
                compile_type_expr_into_validator(right, pointer, false, type_aliases);
            Validator::Primitive {
                pointer: Pointer(pointer.to_string()),
                validation: Validation::Or(
                    Box::new(left_validator.get_validation()),
                    Box::new(right_validator.get_validation()),
                ),
                is_optional,
            }
        }
        TypeExpr::AliasReference(name) => {
            if let Some(alias_type_expr) = type_aliases.get(name) {
                compile_type_expr_into_validator(
                    alias_type_expr,
                    pointer,
                    is_optional,
                    type_aliases,
                )
            } else {
                panic!("Unknown type alias '{}'", name);
            }
        }
    }
}

pub fn compile_schema(
    schema: Schema,
    type_alias_validations: &HashMap<String, TypeExpr>,
) -> Vec<Validator> {
    let mut validators = Vec::new();

    for field in schema.fields {
        let field_pointer = Pointer(format!("/{}", field.name));
        let validator = compile_type_expr_into_validator(
            &field.type_expr,
            field_pointer.0.as_str(),
            field.is_optional,
            type_alias_validations,
        );
        validators.push(validator);
    }

    validators
}

pub fn compile_ast(ast: Ast) -> Vec<Validator> {
    let type_alias_validations: HashMap<String, TypeExpr> = ast
        .type_aliases
        .into_iter()
        .map(|alias| (alias.name.clone(), alias.type_expr))
        .collect();
    compile_schema(ast.schema, &type_alias_validations)
}

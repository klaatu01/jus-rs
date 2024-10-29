use serde_json::Value;
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pointer(pub String);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeValidation {
    String,
    Number,
    Boolean,
    Object,
    Array,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum LiteralValidation {
    String(String),
    Number(String),
    Boolean(bool),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Validation {
    Null,
    Type(TypeValidation),
    Literal(LiteralValidation),
    And(Box<Validation>, Box<Validation>),
    Or(Box<Validation>, Box<Validation>),
}

impl Validation {
    pub fn validate(&self, value: &Value) -> bool {
        match self {
            Validation::Null => value.is_null(),
            Validation::Type(t) => match t {
                TypeValidation::String => value.is_string(),
                TypeValidation::Number => value.is_number(),
                TypeValidation::Boolean => value.is_boolean(),
                TypeValidation::Object => value.is_object(),
                TypeValidation::Array => value.is_array(),
            },
            Validation::Literal(LiteralValidation::String(s)) => {
                value.as_str().map_or(false, |v| v == s)
            }
            Validation::Literal(LiteralValidation::Number(s)) => {
                value.as_str().map_or(false, |v| v == s)
            }
            Validation::Literal(LiteralValidation::Boolean(b)) => {
                value.as_bool().map_or(false, |v| v == *b)
            }
            Validation::And(a, b) => a.validate(value) && b.validate(value),
            Validation::Or(a, b) => a.validate(value) || b.validate(value),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Validator {
    Primitive {
        pointer: Pointer,
        validation: Validation,
        is_optional: bool,
    },
    Object {
        pointer: Pointer,
        field_validators: Vec<Validator>,
        is_optional: bool,
    },
    Array {
        pointer: Pointer,
        element_validator: Box<Validator>,
        is_optional: bool,
    },
}

impl Validator {
    pub fn get_validation(&self) -> Validation {
        match self {
            Validator::Primitive { validation, .. } => validation.clone(),
            Validator::Object {
                field_validators, ..
            } => *field_validators.iter().fold(
                Box::new(Validation::Type(TypeValidation::Object)),
                |acc, field_validator| {
                    Box::new(Validation::And(
                        acc,
                        Box::new(field_validator.get_validation()),
                    ))
                },
            ),
            Validator::Array {
                element_validator, ..
            } => Validation::And(
                Box::new(Validation::Type(TypeValidation::Array)),
                Box::new(element_validator.get_validation()),
            ),
        }
    }

    pub fn validate(&self, value: &Value) -> bool {
        match self {
            Validator::Primitive {
                pointer,
                validation,
                is_optional,
            } => value
                .pointer(&pointer.0)
                .map_or(*is_optional, |v| validation.validate(v)),
            Validator::Object {
                pointer,
                field_validators,
                is_optional,
            } => value.pointer(&pointer.0).map_or(*is_optional, |v| {
                field_validators
                    .iter()
                    .all(|field_validator| field_validator.validate(v))
            }),
            Validator::Array {
                pointer,
                element_validator,
                is_optional,
            } => value.pointer(&pointer.0).map_or(*is_optional, |v| {
                if let Some(array) = v.as_array() {
                    array
                        .iter()
                        .all(|element| element_validator.validate(element))
                } else {
                    false
                }
            }),
        }
    }
}

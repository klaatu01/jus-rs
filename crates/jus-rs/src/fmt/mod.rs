use crate::{
    parser::{Field, LiteralType, PrimitiveType, Schema, TypeAlias, TypeExpr},
    Jus,
};

pub struct Generator<'a> {
    pub file_str: String,
    pub jus: &'a Jus,
}

impl<'a> Generator<'a> {
    pub fn new(jus: &'a Jus) -> Self {
        Self {
            file_str: String::new(),
            jus,
        }
    }

    pub fn generate_primitive_type(&mut self, primitive: &PrimitiveType) {
        match primitive {
            PrimitiveType::String => {
                self.write("string");
            }
            PrimitiveType::Number => {
                self.write("number");
            }
            PrimitiveType::Boolean => {
                self.write("boolean");
            }
        }
    }

    pub fn generate_newline(&mut self) {
        self.file_str.push('\n');
    }

    pub fn generate_semicolon(&mut self) {
        self.file_str.push_str(";\n");
    }

    pub fn generate_array_type(&mut self, item_type: &TypeExpr, indent: usize) {
        self.write("[");
        self.generate_type_expr(item_type, indent + 1);
        self.write("]");
    }

    pub fn generate_type_reference(&mut self, type_alias: &str, _: usize) {
        self.write(type_alias);
    }

    pub fn generate_type_expr(&mut self, type_expr: &TypeExpr, indent: usize) {
        match type_expr {
            TypeExpr::Primitive(primitive) => {
                self.generate_primitive_type(primitive);
            }
            TypeExpr::Literal(literal) => {
                self.generate_literal_type(literal, indent);
            }
            TypeExpr::Object(fields) => {
                self.generate_object_type(fields, indent);
            }
            TypeExpr::Array(item_type) => {
                self.generate_array_type(item_type, indent);
            }
            TypeExpr::AliasReference(name) => {
                self.generate_type_reference(name, indent);
            }
            TypeExpr::Nullable(inner) => {
                self.write_with_indent("!", 0);
                self.generate_type_expr(inner, indent);
            }
            TypeExpr::Union(left, right) => {
                self.generate_type_expr(left, indent);
                self.write_with_indent(" | ", 0);
                self.generate_type_expr(right, indent);
            }
        }
    }

    pub fn write_with_indent(&mut self, s: &str, indent: usize) {
        self.file_str
            .push_str(&format!("{}{}", " ".repeat(indent * 2), s));
    }

    pub fn write(&mut self, s: &str) {
        self.file_str.push_str(s);
    }

    pub fn generate_type_alias(&mut self, type_alias: &TypeAlias, indent: usize) {
        self.write_with_indent(&format!("type {} ", type_alias.name), indent);
        self.generate_type_expr(&type_alias.type_expr, indent);
        self.generate_semicolon();
        self.generate_newline();
    }

    pub fn generate_literal_type(&mut self, literal: &LiteralType, _: usize) {
        match literal {
            LiteralType::String(s) => {
                self.write(&format!("\"{}\"", s));
            }
            LiteralType::Number(n) => {
                self.write(&n.to_string());
            }
            LiteralType::Boolean(b) => {
                self.write(&format!("{}", b));
            }
        }
    }

    pub fn generate_object_type(&mut self, fields: &[Field], indent: usize) {
        self.write("{");
        self.generate_newline();
        for field in fields {
            let optional = if field.is_optional { "?" } else { "" };
            self.write_with_indent(&format!("{}{}: ", field.name, optional), indent + 1);
            self.generate_type_expr(&field.type_expr, indent + 1);
            self.generate_semicolon();
        }
        self.write_with_indent("}", indent);
    }

    pub fn generate_schema(&mut self, schema: &Schema) {
        self.write_with_indent(&format!("schema {} ", schema.name), 0);
        self.generate_object_type(&schema.fields, 0);
        self.generate_newline();
    }

    pub fn generate(mut self) -> String {
        self.jus
            .ast
            .type_aliases
            .iter()
            .for_each(|type_alias| self.generate_type_alias(type_alias, 0));

        self.generate_schema(&self.jus.ast.schema);

        self.file_str
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_complex() {
        let input = include_str!("../../data/schema/simple.jus");
        let jus = Jus::compile(input).unwrap();
        let generator = Generator::new(&jus);
        let result = generator.generate();
        assert_eq!(result, input);
    }

    #[test]
    fn test_generate_complex_with_array() {
        let input = include_str!("../../data/schema/complex.jus");
        let jus = Jus::compile(input).unwrap();
        let generator = Generator::new(&jus);
        let result = generator.generate();
        println!("{}", result);
        assert_eq!(result, input);
    }

    #[test]
    fn test_format_and_parse_complex() {
        let input = include_str!("../../data/schema/complex.jus");
        let jus = Jus::compile(input).unwrap();
        let generator = Generator::new(&jus);
        let result = generator.generate();
        let jus = Jus::compile(&result).unwrap();
        let generator = Generator::new(&jus);
        let resultb = generator.generate();
        assert_eq!(result, resultb);
    }
}

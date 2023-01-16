#[derive(Clone, Debug)]
pub enum Type {
    Float,
    Int,
    Bool,
    String,
    Struct(Vec<String>),
    Nil
}

#[derive(Clone, Debug)]
pub struct TypeInfo {
    kind: Option<Type>
}

impl TypeInfo {
    pub fn new(kind: Type) -> Self {
        TypeInfo {
            kind: Some(kind),
        }
    }

    pub fn kind(&self) -> &Option<Type> {
        &self.kind
    }

    pub fn structure(keys: Vec<String>) -> Self {
        TypeInfo {
            kind: Some(Type::Struct(keys)),
        }
    }

    pub fn nil() -> Self {
        TypeInfo {
            kind: None,
        }
    }
}
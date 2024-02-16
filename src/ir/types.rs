#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Float,
    Int,
    Bool,
    String,
    Struct(Vec<String>),
    /// Absence of value (for statements and None/Nil in some languages)
    Nil,
    /// Any type
    Any,
}

impl Type {
    pub fn as_info(self) -> TypeInfo {
        TypeInfo { kind: self }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeInfo {
    kind: Type,
}

impl TypeInfo {
    pub fn new(kind: Type) -> Self {
        TypeInfo { kind }
    }

    pub fn kind(&self) -> &Type {
        &self.kind
    }

    pub fn structure(keys: Vec<String>) -> Self {
        TypeInfo {
            kind: Type::Struct(keys),
        }
    }

    pub fn any() -> Self {
        TypeInfo { kind: Type::Any}
    }

    pub fn nil() -> Self {
        TypeInfo { kind: Type::Nil }
    }

    pub fn is_numerical(&self) -> bool {
        self.kind == Type::Float || self.kind == Type::Any
    }
}

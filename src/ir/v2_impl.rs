use super::{Expr, ExprNode, Generate, IrBuilder, Literal, Type, TypeInfo};

/// Implementations of Generate for rust types (bool, f64...)

impl Generate for u32 {
    fn generate(&self, context: &mut IrBuilder) -> ExprNode {
        Expr::Literal(Literal::Number(self.clone().into())).node(TypeInfo::new(Type::Float))
    }

    fn type_info(&self, context: &IrBuilder) -> TypeInfo {
        TypeInfo::new(Type::Float)
    }
}

impl Generate for f32 {
    fn generate(&self, context: &mut IrBuilder) -> ExprNode {
        Expr::Literal(Literal::Number(self.clone().into())).node(TypeInfo::new(Type::Float))
    }

    fn type_info(&self, context: &IrBuilder) -> TypeInfo {
        TypeInfo::new(Type::Float)
    }
}

impl Generate for f64 {
    fn generate(&self, context: &mut IrBuilder) -> ExprNode {
        Expr::Literal(Literal::Number(self.clone().into())).node(TypeInfo::new(Type::Float))
    }

    fn type_info(&self, context: &IrBuilder) -> TypeInfo {
        TypeInfo::new(Type::Float)
    }
}

impl Generate for bool {
    fn generate(&self, context: &mut IrBuilder) -> ExprNode {
        Expr::Literal(Literal::Boolean(self.clone())).node(TypeInfo::new(Type::Bool))
    }

    fn type_info(&self, context: &IrBuilder) -> TypeInfo {
        TypeInfo::new(Type::Bool)
    }
}
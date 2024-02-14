use std::ops::{Add, Div, Mul, Rem, Sub};

use super::{BinaryOp, Binding, Expr, ExprNode, IrBuilder, Literal, Type, TypeInfo};

macro_rules! impl_partial_cmp {
    ($method_name:ident, $bin_op:expr) => {
        fn $method_name<'v>(&'v self, other: &'v dyn Generate) -> BinaryOperation
        where
            Self: Generate + Sized,
        {
            BinaryOperation {
                lhs: Box::new(self),
                operator: $bin_op,
                rhs: Box::new(other),
            }
        }
    };
}

pub trait Generate {
    fn generate(&self, context: &mut IrBuilder) -> ExprNode;
    fn type_info(&self, context: &IrBuilder) -> TypeInfo;
    fn emit(&self, context: &mut IrBuilder) {
        let gen = self.generate(context);
        context.program.push(gen);
    }

    impl_partial_cmp!(equals, BinaryOp::Equal);
    impl_partial_cmp!(not_equals, BinaryOp::NEqual);
    impl_partial_cmp!(lt, BinaryOp::Lt);
    impl_partial_cmp!(lte, BinaryOp::LtEqual);
    impl_partial_cmp!(gt, BinaryOp::Gt);
    impl_partial_cmp!(gte, BinaryOp::GtEqual);
}

#[derive(Clone)]
pub struct Variable {
    name: String,
    depth: Option<(usize, usize)>,
}

impl Variable {
    pub fn global(name: impl ToString) -> Self {
        Variable {
            name: name.to_string(),
            depth: None,
        }
    }

    pub fn local(name: impl ToString, depth: (usize, usize)) -> Self {
        Variable {
            name: name.to_string(),
            depth: Some(depth),
        }
    }

    pub fn binding(&self) -> Binding {
        match self.depth {
            None => Binding::global(&self.name),
            Some((depth, function_depth)) => Binding::local(&self.name, depth, function_depth),
        }
    }

    pub fn bind<'v>(&self, value: impl Generate + 'v) -> Assignement<'v> {
        Assignement {
            variable: self.clone(),
            value: Box::new(value),
        }
    }
}

impl Generate for Variable {
    fn generate(&self, context: &mut IrBuilder) -> ExprNode {
        let type_info = context
            .types
            .last()
            .unwrap()
            .get(&self.name)
            .unwrap_or(&TypeInfo::nil())
            .clone();

        match self.depth {
            None => Expr::Var(Binding::global(&self.name)).node(type_info),
            Some((depth, function_depth)) => {
                Expr::Var(Binding::local(&self.name, depth, function_depth)).node(type_info)
            }
        }
    }

    fn type_info(&self, context: &IrBuilder) -> TypeInfo {
        TypeInfo::nil()
    }
}

// To make the api nicer Assignement
// doesn't take a reference to the value
// this comes at the cost of not being
// able to clone the struct, this doesn't
// seem necessary but if you think so feel
// free to open an issue
pub struct Assignement<'v> {
    variable: Variable,
    value: Box<dyn Generate + 'v>,
}

impl<'v> Generate for Assignement<'v> {
    fn generate(&self, context: &mut IrBuilder) -> ExprNode {
        let binding = self.variable.binding();

        let type_info = self.value.type_info(&context).clone();

        let map = context
            .types
            .get_mut(binding.clone().depth.unwrap_or(0) + binding.function_depth)
            .unwrap();

        map.insert(binding.name().into(), type_info);

        Expr::Bind(binding.clone(), self.value.generate(context)).node(TypeInfo::nil())
    }

    fn type_info(&self, context: &IrBuilder) -> TypeInfo {
        TypeInfo::nil()
    }
}

impl<'a, T> Generate for T
where
    f64: From<T>,
    T: Clone + Copy,
{
    fn generate(&self, _: &mut IrBuilder) -> ExprNode {
        let info = TypeInfo::new(Type::Float);
        let val = self.clone().clone();
        let val = Into::<f64>::into(val);
        let lit = Literal::Number(val);

        Expr::Literal(lit).node(info)
    }

    fn type_info(&self, context: &IrBuilder) -> TypeInfo {
        TypeInfo::new(Type::Float)
    }
}

pub struct BinaryOperation<'v> {
    lhs: Box<&'v dyn Generate>,
    rhs: Box<&'v dyn Generate>,
    operator: BinaryOp,
}

impl<'v> Generate for BinaryOperation<'v> {
    fn generate(&self, context: &mut IrBuilder) -> ExprNode {
        let lhs = self.lhs.generate(context);
        let rhs = self.rhs.generate(context);

        context.binary(lhs, self.operator.clone(), rhs)
    }

    fn type_info(&self, context: &IrBuilder) -> TypeInfo {
        self.lhs.type_info(context)
    }
}

macro_rules! impl_operations {
    ($target:tt => $($imp:tt),+) => {
        $(impl_operation_generic!($target, $imp);)+
    };
}

macro_rules! impl_operation_generic {
    ($target:tt, Numerical) => {
        impl_operation_generic!($target, +);
        impl_operation_generic!($target, -);
        impl_operation_generic!($target, *);
        impl_operation_generic!($target, /);
        impl_operation_generic!($target, %);
    };

    ($target:tt, +) => {
        impl_operation!($target, add, Add, Add);
    };
    ($target:tt, -) => {
        impl_operation!($target, sub, Sub, Sub);
    };
    ($target:tt, *) => {
        impl_operation!($target, mul, Mul, Mul);
    };
    ($target:tt, /) => {
        impl_operation!($target, div, Div, Div);
    };
    ($target:tt, %) => {
        impl_operation!($target, rem, Rem, Rem);
    };
}

macro_rules! impl_operation {
    ($target:tt, $method_name:tt, $trait_name:ident, $bin_op_variant:tt) => {
        impl<'v, T> $trait_name<&'v T> for &'v $target
        where
            T: Generate + 'v,
        {
            type Output = BinaryOperation<'v>;

            fn $method_name(self, rhs: &'v T) -> Self::Output {
                BinaryOperation {
                    lhs: Box::new(self),
                    rhs: Box::new(rhs),
                    operator: BinaryOp::$bin_op_variant,
                }
            }
        }
    };
}

impl_operations!(Variable => Numerical);

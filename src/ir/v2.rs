use std::{
    cell::RefCell,
    fmt::Debug,
    ops::{Add, Deref, Div, Mul, Rem, Sub},
    rc::Rc,
};

use super::{
    BinaryOp, Binding, Expr, ExprNode, IrBuilder, IrFunction, IrFunctionBody, Literal, Type,
    TypeInfo,
};

pub trait Generate: Debug {
    fn generate(&self, context: &mut IrBuilder) -> ExprNode;
    fn type_info(&self, context: &IrBuilder) -> TypeInfo;
    fn emit(&self, context: &mut IrBuilder) {
        let gen = self.generate(context);
        context.program.push(gen);
    }

    fn boxed(self) -> Box<Self>
    where
        Self: Generate + Sized,
    {
        Box::new(self)
    }

    fn ret(self) -> Return<Self>
    where
        Self: Generate + Sized,
    {
        Return { value: self }
    }

    fn call<'v>(self, args: Vec<Box<dyn Generate>>) -> Call<Self>
    where
        Self: Generate + Sized,
    {
        Call { callee: self, args }
    }
}

#[derive(Clone, Debug)]
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

    pub fn bind<T>(&self, value: T) -> Assignement<T>
    where
        T: Generate,
    {
        Assignement {
            variable: self.clone(),
            value,
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
#[derive(Debug)]
pub struct Assignement<T> {
    variable: Variable,
    value: T,
}

impl<T> Generate for Assignement<T>
where
    T: Generate,
{
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
    T: Clone + Copy + Debug,
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

#[derive(Debug)]
pub struct Call<T> {
    callee: T,
    args: Vec<Box<dyn Generate>>,
}

impl<T> Call<T> {
    pub fn new(callee: T, args: Vec<Box<dyn Generate>>) -> Self {
        Self { callee, args }
    }
}

impl<T> Generate for Call<T>
where
    T: Generate,
{
    fn generate(&self, context: &mut IrBuilder) -> ExprNode {
        let callee = self.callee.generate(context);
        let args = self
            .args
            .iter()
            .map(|elem| elem.generate(context))
            .collect();

        Expr::Call(super::ir::Call { callee, args }).node(self.callee.type_info(context))
    }

    fn type_info(&self, context: &IrBuilder) -> TypeInfo {
        self.callee.type_info(context)
    }
}

#[derive(Debug)]
pub struct Return<T> {
    value: T,
}

impl<T> Return<T> {
    pub fn new(value: T) -> Self {
        Self { value }
    }
}

impl<T> Generate for Return<T>
where
    T: Generate,
{
    fn generate(&self, context: &mut IrBuilder) -> ExprNode {
        let type_info = self.value.type_info(context);
        Expr::Return(Some(self.value.generate(context))).node(type_info)
    }

    fn type_info(&self, context: &IrBuilder) -> TypeInfo {
        self.value.type_info(context)
    }
}

pub struct Function<T> {
    variable: Variable,
    args: Vec<String>,
    body: T,
}

impl<T> Function<T> {
    pub fn new<'a>(
        variable: Variable,
        args: Vec<&'a str>,
        body: T,
    ) -> Self
    where
        T: Fn(&mut IrBuilder),
    {
        Self {
            variable,
            args: args.iter().map(|it| it.to_string()).collect(),
            body,
        }
    }
}

impl<T> Debug for Function<T> {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
       f.debug_struct("Function")
        .field("variable", &self.variable)
        .field("args", &self.args)
        .field("function", &"<...>")
        .finish()
   } 
}

impl<T> Generate for Function<T>
where
    T: Fn(&mut IrBuilder),
{
    fn generate(&self, context: &mut IrBuilder) -> ExprNode {
        let mut body_builder = IrBuilder::new();
        (self.body)(&mut body_builder);

        context.scope_in();
        let body = body_builder.build();
        context.scope_out();

        let func_body = IrFunctionBody {
            params: self
                .args
                .iter()
                .cloned()
                .map(|x| {
                    Binding::local(
                        &x,
                        self.variable.binding().depth.unwrap_or(0) + 1,
                        self.variable.binding().function_depth + 1,
                    )
                })
                .collect::<Vec<Binding>>(),
            method: false,
            inner: body,
        };

        let ir_func = IrFunction {
            var: self.variable.binding(),
            body: Rc::new(RefCell::new(func_body)),
        };

        Expr::Function(ir_func).node(TypeInfo::nil())
    }

    fn type_info(&self, context: &IrBuilder) -> TypeInfo {
        TypeInfo::nil()
    }
}

#[derive(Debug)]
pub struct BinaryOperation<L, R> {
    lhs: L,
    rhs: R,
    operator: BinaryOp,
}

impl<L, R> Generate for BinaryOperation<L, R>
where
    L: Generate,
    R: Generate,
{
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
    ($target:tt<$($generics:ident),*> => $imp:tt) => {
        impl_operation_generic!($target<$($generics),*>, $imp);
    };
}

macro_rules! impl_operation_generic {
    ($target:tt<$($generics:ident),*>, Numerical) => {
        impl_operation_generic!($target<$($generics),*>, +);
        impl_operation_generic!($target<$($generics),*>, -);
        impl_operation_generic!($target<$($generics),*>, *);
        impl_operation_generic!($target<$($generics),*>, /);
        impl_operation_generic!($target<$($generics),*>, %);
    };

    ($target:tt<$($generics:ident),*>, PartialEq) => {
        impl_partial_cmp!($target<$($generics),*>, equals, BinaryOp::Equal);
        impl_partial_cmp!($target<$($generics),*>, not_equals, BinaryOp::NEqual);
    };

    ($target:ident<$($generics:ident),*>, PartialOrd) => {
        impl_partial_cmp!($target<$($generics),*>, lt, BinaryOp::Lt);
        impl_partial_cmp!($target<$($generics),*>, lte, BinaryOp::LtEqual);
        impl_partial_cmp!($target<$($generics),*>, gt, BinaryOp::Gt);
        impl_partial_cmp!($target<$($generics),*>, gte, BinaryOp::GtEqual);
    };

    ($target:tt<$($generics:ident),*>, +) => {
        impl_operation!($target<$($generics),*>, add, Add, Add);
    };
    ($target:tt<$($generics:ident),*>, -) => {
        impl_operation!($target<$($generics),*>, sub, Sub, Sub);
    };
    ($target:tt<$($generics:ident),*>, *) => {
        impl_operation!($target<$($generics),*>, mul, Mul, Mul);
    };
    ($target:tt<$($generics:ident),*>, /) => {
        impl_operation!($target<$($generics),*>, div, Div, Div);
    };
    ($target:tt<$($generics:ident),*>, %) => {
        impl_operation!($target<$($generics),*>, rem, Rem, Rem);
    };
}

macro_rules! impl_partial_cmp {
    ($target:tt<$($generics:ident),*>, $method_name:ident, $bin_op:expr) => {
        impl<$($generics),*> $target<$($generics),*> {
            /// Compares two implementors of the Generate trait
            pub fn $method_name<R>(self, other: R) -> BinaryOperation<Self, R>
            where
                Self: Generate,
            {
                BinaryOperation {
                    lhs: self,
                    operator: $bin_op,
                    rhs: other,
                }
            }
        }
    };
}

macro_rules! impl_operation {
    ($target:tt<$($generics:ident),*>, $method_name:tt, $trait_name:ident, $bin_op_variant:tt) => {
        impl<R, $($generics),*> $trait_name<R> for $target<$($generics),*>
        where
            R: Generate,
        {
            type Output = BinaryOperation<$target<$($generics),*>, R>;

            fn $method_name(self, rhs: R) -> Self::Output {
                BinaryOperation {
                    lhs: self,
                    rhs,
                    operator: BinaryOp::$bin_op_variant,
                }
            }
        }
    };
}

impl_operations!(Variable<> => PartialEq);
impl_operations!(Variable<> => PartialOrd);
impl_operations!(Variable<> => Numerical);

impl_operations!(BinaryOperation<A, B> => PartialEq);
impl_operations!(BinaryOperation<A, B> => PartialOrd);
impl_operations!(BinaryOperation<A, B> => Numerical);

impl_operations!(Call<A> => PartialEq);
impl_operations!(Call<A> => PartialOrd);
impl_operations!(Call<A> => Numerical);

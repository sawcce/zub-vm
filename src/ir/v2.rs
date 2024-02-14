use super::{Binding, Expr, ExprNode, IrBuilder, Literal, Type, TypeInfo};

pub trait Generate {
    fn generate(&self, context: &mut IrBuilder) -> ExprNode;
    fn type_info(&self, context: &IrBuilder) -> TypeInfo;
    fn emit(&self, context: &mut IrBuilder) {
        let gen = self.generate(context);
        context.program.push(gen);
    }
}

#[derive(Clone)]
pub struct Variable {
    name: String,
    depth: Option<(usize, usize)>,
}

impl Variable {
    pub fn global(name: impl ToString) -> Self {
        Variable { name: name.to_string(), depth: None }
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

    pub fn bind<'v>(&self, value: &'v dyn Generate) -> Assignement<'v> {
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

#[derive(Clone)]
pub struct Assignement<'v> {
    variable: Variable,
    value: Box<&'v dyn Generate>,
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

    fn type_info(&self, context: & IrBuilder) -> TypeInfo {
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
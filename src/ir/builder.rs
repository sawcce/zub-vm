use super::*;

use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryInto;
use std::rc::Rc;



#[derive(Clone, Debug)]
pub struct IrBuilder {
    pub(super) program: Vec<ExprNode>,
    pub(super) types: Vec<HashMap<String, TypeInfo>>,
}

impl IrBuilder {
    pub fn new() -> Self {
        IrBuilder {
            program: Vec::new(),
            types: vec![HashMap::new()],
        }
    }


    pub fn bind(&mut self, binding: Binding, rhs: ExprNode) {
        let map = self
            .types
            .get_mut(binding.clone().depth.unwrap_or(0) + binding.function_depth)
            .unwrap();
        map.insert(binding.name().into(), rhs.type_info().clone());

        let bind = Expr::Bind(binding.clone(), rhs);
        self.emit(bind.node(TypeInfo::nil()));
    }

    pub fn scope_in(&mut self) {
        self.types.push(HashMap::new());
    }

    pub fn scope_out(&mut self) {
        self.types.pop();
    }

    pub fn mutate(&mut self, lhs: ExprNode, rhs: ExprNode) {
        let mutate = Expr::Mutate(lhs, rhs);

        self.emit(mutate.clone().node(TypeInfo::nil()))
    }

    pub fn ret_(&mut self, value: Option<ExprNode>) -> ExprNode {
        let info = if let Some(ref value) = value {
            value.type_info().clone()
        } else {
            TypeInfo::nil()
        };

        Expr::Return(value).node(info)
    }

    pub fn ret(&mut self, value: Option<ExprNode>) {
        let node = self.ret_(value);
        self.emit(node);
    }

    pub fn break_(&mut self) {
        self.emit(Expr::Break.node(TypeInfo::nil()))
    }

    pub fn list(&self, content: Vec<ExprNode>) -> ExprNode {
        Expr::List(content).node(TypeInfo::nil())
    }

    pub fn set_element(&self, list: ExprNode, index: ExprNode, value: ExprNode) -> ExprNode {
        Expr::SetElement(list, index, value).node(TypeInfo::nil())
    }

    pub fn get_element(&self, list: ExprNode, index: ExprNode) -> ExprNode {
        Expr::GetElement(list, index).node(TypeInfo::nil())
    }

    pub fn tuple(&self, members: Vec<ExprNode>) -> ExprNode {
        Expr::Tuple(members).node(TypeInfo::nil())
    }

    pub fn structure(&self, keys: Vec<String>, values: Vec<ExprNode>) -> ExprNode {
        Expr::Tuple(values).node(TypeInfo::structure(keys))
    }

    pub fn get_member(&self, key: String, structure: ExprNode) -> ExprNode {
        use types::Type::Struct;

        if let Struct(ref keys) = structure.type_info().kind() {
            let index = keys.iter().position(|e| e == &key).unwrap();
            let index = self.number(index as f64);
            Expr::GetElement(structure, index).node(TypeInfo::nil())
        } else {
            panic!("Expected struct!");
        }
    }

    pub fn set_member(&self, key: String, structure: ExprNode, value: ExprNode) -> ExprNode {
        use types::Type::Struct;

        if let Struct(ref keys) = structure.type_info().kind() {
            let index = keys.iter().position(|e| e == &key).unwrap();
            let index = self.number(index as f64);
            Expr::SetElement(structure, index, value).node(TypeInfo::nil())
        } else {
            panic!("Expected struct!");
        }
    }

    pub fn dict(&self, keys: Vec<ExprNode>, values: Vec<ExprNode>) -> ExprNode {
        Expr::Dict(keys, values).node(TypeInfo::nil())
    }

    pub fn empty_dict(&self) -> ExprNode {
        Expr::Dict(Vec::new(), Vec::new()).node(TypeInfo::nil())
    }

    pub fn var(&self, binding: Binding) -> ExprNode {
        let a = self
            .types
            .last()
            .unwrap()
            .get(binding.name())
            .unwrap_or(&TypeInfo::nil())
            .clone();
        Expr::Var(binding).node(a)
    }

    pub fn call(&self, callee: ExprNode, args: Vec<ExprNode>, retty: Option<TypeInfo>) -> ExprNode {
        let call = super::ir::Call { callee, args };

        Expr::Call(call).node(if let Some(info) = retty {
            info
        } else {
            TypeInfo::nil()
        })
    }

    pub fn binary(&self, lhs: ExprNode, op: BinaryOp, rhs: ExprNode) -> ExprNode {
        Expr::Binary(lhs, op, rhs).node(TypeInfo::nil())
    }

    pub fn unary(op: UnaryOp, rhs: ExprNode) -> Expr {
        Expr::Unary(op, rhs)
    }

    pub fn int(&self, n: i32) -> ExprNode {
        let info = TypeInfo::new(Type::Int);
        let lit = Literal::Number(n as f64);

        Expr::Literal(lit).node(info)
    }

    pub fn number(&self, n: f64) -> ExprNode {
        let info = TypeInfo::new(Type::Float);
        let lit = Literal::Number(n);

        Expr::Literal(lit).node(info)
    }

    pub fn string(&self, s: &str) -> ExprNode {
        let info = TypeInfo::new(Type::String);
        let lit = Literal::String(s.to_owned());

        Expr::Literal(lit).node(info)
    }

    pub fn bool(&self, b: bool) -> ExprNode {
        let info = TypeInfo::new(Type::Bool);
        let lit = Literal::Boolean(b);

        Expr::Literal(lit).node(info)
    }

    pub fn nil(&self) -> ExprNode {
        let info = TypeInfo::new(Type::Nil);
        let lit = Literal::Nil;

        Expr::Literal(lit).node(info)
    }

    pub fn function(
        &mut self,
        var: Binding,
        params: &[&str],
        mut body_build: impl FnMut(&mut IrBuilder),
    ) -> ExprNode {
        let mut body_builder = IrBuilder::new();

        body_build(&mut body_builder);

        self.scope_in();
        let body = body_builder.build();
        // println!("Body: {body:?}");
        self.scope_out();

        let func_body = IrFunctionBody {
            params: params
                .iter()
                .cloned()
                .map(|x: &str| {
                    Binding::local(x, var.depth.unwrap_or(0) + 1, var.function_depth + 1)
                })
                .collect::<Vec<Binding>>(),
            method: false,
            inner: body,
        };

        let ir_func = IrFunction {
            var,
            body: Rc::new(RefCell::new(func_body)),
        };

        Expr::Function(ir_func).node(TypeInfo::nil())
    }

    pub fn ternary(
        &mut self,
        cond: ExprNode,
        then_body: ExprNode,
        else_body: Option<ExprNode>,
    ) -> ExprNode {
        Expr::If(cond, then_body, vec![], else_body).node(TypeInfo::nil())
    }

    pub fn block(&mut self, body: Vec<ExprNode>) -> ExprNode {
        Expr::Block(body).node(TypeInfo::nil())
    }

    pub fn block_(&mut self, build: impl FnOnce(&mut IrBuilder)) -> ExprNode {
        let builder = &mut IrBuilder::new();
        build(builder);

        Expr::Block(builder.build()).node(TypeInfo::nil())
    }

    pub fn if_(
        &mut self,
        cond: ExprNode,
        then_build: impl FnOnce(&mut IrBuilder),
        else_build: Option<impl FnOnce(&mut IrBuilder)>,
    ) -> ExprNode {
        let mut then_builder = IrBuilder::new();

        then_build(&mut then_builder);

        let then_body = Expr::Block(then_builder.build()).node(TypeInfo::nil());

        let else_body = if let Some(else_build) = else_build {
            let mut else_builder = IrBuilder::new();

            else_build(&mut else_builder);

            Some(Expr::Block(else_builder.build()).node(TypeInfo::nil()))
        } else {
            None
        };

        Expr::If(cond, then_body, vec![], else_body).node(TypeInfo::nil())
    }

    pub fn while_(&mut self, cond: ExprNode, then_build: fn(&mut IrBuilder)) -> ExprNode {
        let mut then_builder = IrBuilder::new();

        then_build(&mut then_builder);

        let then_body = Expr::Block(then_builder.build()).node(TypeInfo::nil());

        Expr::While(cond, then_body).node(TypeInfo::nil())
    }

    pub fn build(&self) -> Vec<ExprNode> {
        self.program.clone()
    }

    pub fn emit(&mut self, atom: ExprNode) {
        self.program.push(atom)
    }
}

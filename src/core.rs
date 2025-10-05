use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Arrow(Box<Type>, Box<Type>),
}

// Terms in STLC with named variables
#[derive(Debug, Clone)]
pub enum Expr {
    Var(String),
    Lam(String, Type, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Int(i64),
    Bool(bool),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Fix(String, Type, Box<Expr>),
}

pub type Env = HashMap<usize, Rc<Value>>;

// Runtime values (use De Bruijn indices)
#[derive(Clone)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Closure(Rc<dyn Fn(Rc<Value>) -> Rc<Value>>),
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "Int({})", n),
            Value::Bool(b) => write!(f, "Bool({})", b),
            Value::Closure(_) => write!(f, "<closure>"),
        }
    }
}

pub type TypeContext = HashMap<String, Type>;

// Conversion from named terms to De Bruijn indices
pub struct DeBruijnConverter {
    var_stack: Vec<String>,
}

impl DeBruijnConverter {
    pub fn new() -> Self {
        Self {
            var_stack: Vec::new(),
        }
    }

    pub fn convert_term(&mut self, term: &Expr) -> Term {
        match term {
            Expr::Var(name) => {
                // Find the variable in the stack (search from the end/most recent)
                for (depth, var) in self.var_stack.iter().rev().enumerate() {
                    if var == name {
                        return Term::Var(depth);
                    }
                }
                panic!("Unbound variable: {}", name);
            }
            Expr::Lam(param, param_type, body) => {
                self.var_stack.push(param.clone());
                let body_db = self.convert_term(body);
                self.var_stack.pop();
                Term::Lam(param_type.clone(), Box::new(body_db))
            }
            Expr::App(t1, t2) => {
                Term::App(
                    Box::new(self.convert_term(t1)),
                    Box::new(self.convert_term(t2)),
                )
            }
            Expr::Int(n) => Term::Int(*n),
            Expr::Bool(b) => Term::Bool(*b),
            Expr::Add(t1, t2) => {
                Term::Add(
                    Box::new(self.convert_term(t1)),
                    Box::new(self.convert_term(t2)),
                )
            }
            Expr::Sub(t1, t2) => {
                Term::Sub(
                    Box::new(self.convert_term(t1)),
                    Box::new(self.convert_term(t2)),
                )
            }
            Expr::Mul(t1, t2) => {
                Term::Mul(
                    Box::new(self.convert_term(t1)),
                    Box::new(self.convert_term(t2)),
                )
            }
            Expr::Eq(t1, t2) => {
                Term::Eq(
                    Box::new(self.convert_term(t1)),
                    Box::new(self.convert_term(t2)),
                )
            }
            Expr::Lt(t1, t2) => {
                Term::Lt(
                    Box::new(self.convert_term(t1)),
                    Box::new(self.convert_term(t2)),
                )
            }
            Expr::Gt(t1, t2) => {
                Term::Gt(
                    Box::new(self.convert_term(t1)),
                    Box::new(self.convert_term(t2)),
                )
            }
            Expr::If(cond, then_branch, else_branch) => {
                Term::If(
                    Box::new(self.convert_term(cond)),
                    Box::new(self.convert_term(then_branch)),
                    Box::new(self.convert_term(else_branch)),
                )
            }
            Expr::Fix(name, fix_type, body) => {
                self.var_stack.push(name.clone());
                let body_db = self.convert_term(body);
                self.var_stack.pop();
                Term::Fix(fix_type.clone(), Box::new(body_db))
            }
        }
    }
}

// Terms with De Bruijn indices
#[derive(Debug, Clone)]
pub enum Term {
    Var(usize),
    Lam(Type, Box<Term>),
    App(Box<Term>, Box<Term>),
    Int(i64),
    Bool(bool),
    Add(Box<Term>, Box<Term>),
    Sub(Box<Term>, Box<Term>),
    Mul(Box<Term>, Box<Term>),
    Eq(Box<Term>, Box<Term>),
    Lt(Box<Term>, Box<Term>),
    Gt(Box<Term>, Box<Term>),
    If(Box<Term>, Box<Term>, Box<Term>),
    Fix(Type, Box<Term>),
}

// Regular recursive interpreter
pub fn eval(term: &Term, env: &Env) -> Rc<Value> {
    match term {
        Term::Var(x) => env.get(x).unwrap().clone(),
        Term::Lam(_param_type, body) => {
            let body = body.clone();
            let env = env.clone();
            Rc::new(Value::Closure(Rc::new(move |arg| {
                // Shift all existing indices up by 1 and insert new binding at 0
                let mut new_env = Env::new();
                for (idx, val) in env.iter() {
                    new_env.insert(idx + 1, val.clone());
                }
                new_env.insert(0, arg);
                eval(&body, &new_env)
            })))
        }
        Term::App(t1, t2) => {
            let v1 = eval(t1, env);
            let v2 = eval(t2, env);
            match &*v1 {
                Value::Closure(f) => f(v2),
                _ => panic!("Runtime type error: expected closure"),
            }
        }
        Term::Int(n) => Rc::new(Value::Int(*n)),
        Term::Bool(b) => Rc::new(Value::Bool(*b)),
        Term::Add(t1, t2) => {
            let v1 = eval(t1, env);
            let v2 = eval(t2, env);
            match (&*v1, &*v2) {
                (Value::Int(n1), Value::Int(n2)) => Rc::new(Value::Int(n1 + n2)),
                _ => panic!("Addition on non-integers"),
            }
        }
        Term::Sub(t1, t2) => {
            let v1 = eval(t1, env);
            let v2 = eval(t2, env);
            match (&*v1, &*v2) {
                (Value::Int(n1), Value::Int(n2)) => Rc::new(Value::Int(n1 - n2)),
                _ => panic!("Subtraction on non-integers"),
            }
        }
        Term::Mul(t1, t2) => {
            let v1 = eval(t1, env);
            let v2 = eval(t2, env);
            match (&*v1, &*v2) {
                (Value::Int(n1), Value::Int(n2)) => Rc::new(Value::Int(n1 * n2)),
                _ => panic!("Multiplication on non-integers"),
            }
        }
        Term::Eq(t1, t2) => {
            let v1 = eval(t1, env);
            let v2 = eval(t2, env);
            match (&*v1, &*v2) {
                (Value::Int(n1), Value::Int(n2)) => Rc::new(Value::Bool(n1 == n2)),
                (Value::Bool(b1), Value::Bool(b2)) => Rc::new(Value::Bool(b1 == b2)),
                _ => panic!("Equality check on incompatible types"),
            }
        }
        Term::Lt(t1, t2) => {
            let v1 = eval(t1, env);
            let v2 = eval(t2, env);
            match (&*v1, &*v2) {
                (Value::Int(n1), Value::Int(n2)) => Rc::new(Value::Bool(n1 < n2)),
                _ => panic!("Less than check on non-integers"),
            }
        }
        Term::Gt(t1, t2) => {
            let v1 = eval(t1, env);
            let v2 = eval(t2, env);
            match (&*v1, &*v2) {
                (Value::Int(n1), Value::Int(n2)) => Rc::new(Value::Bool(n1 > n2)),
                _ => panic!("Greater than check on non-integers"),
            }
        }
        Term::If(cond, then_branch, else_branch) => {
            let cond_val = eval(cond, env);
            match &*cond_val {
                Value::Bool(true) => eval(then_branch, env),
                Value::Bool(false) => eval(else_branch, env),
                _ => panic!("If condition must be boolean"),
            }
        }
        Term::Fix(_annotated_type, body) => {
            // Create a recursive closure using the Y-combinator approach
            // fix f. body  evaluates body with f bound to the fixed point
            let body_clone = body.clone();
            let env_clone = env.clone();

            let rec_closure: Rc<std::cell::RefCell<Option<Rc<Value>>>> = Rc::new(std::cell::RefCell::new(None));
            let rec_closure_ref = rec_closure.clone();

            // Evaluate body with the recursive binding in the environment
            let mut fix_env = env_clone.clone();
            // Shift existing indices
            for (idx, val) in env_clone.iter() {
                fix_env.insert(idx + 1, val.clone());
            }

            // Create the recursive closure placeholder
            let self_ref = Rc::new(Value::Closure(Rc::new(move |arg| {
                let func: Rc<Value> = rec_closure_ref.borrow().clone().unwrap();
                if let Value::Closure(f) = &*func {
                    f(arg)
                } else {
                    panic!("Fixpoint should be a closure")
                }
            })));

            fix_env.insert(0, self_ref);
            let result = eval(&body_clone, &fix_env);
            *rec_closure.borrow_mut() = Some(result.clone());

            result
        }
    }
}

// Trampoline-based interpreter
enum State {
    Compute(Term, Env),
    Continue(Rc<Value>),
}

enum Frame {
    AppFun(Term, Env),
    AppArg(Rc<Value>),
    AddLeft(Term, Env),
    AddRight(Rc<Value>),
    SubLeft(Term, Env),
    SubRight(Rc<Value>),
    MulLeft(Term, Env),
    MulRight(Rc<Value>),
    EqLeft(Term, Env),
    EqRight(Rc<Value>),
    LtLeft(Term, Env),
    LtRight(Rc<Value>),
    GtLeft(Term, Env),
    GtRight(Rc<Value>),
    IfBranch(Term, Term, Env),
}

pub fn eval_trampolined(term: &Term, env: &Env) -> Rc<Value> {
    let mut state = State::Compute(term.clone(), env.clone());
    let mut stack: Vec<Frame> = Vec::new();

    loop {
        match state {
            State::Compute(term, env) => {
                match term {
                    Term::Var(x) => {
                        let value = env.get(&x).unwrap().clone();
                        state = State::Continue(value);
                    }
                    Term::Lam(_, body) => {
                        let body_clone = (*body).clone();
                        let env_clone = env.clone();
                        let closure = Rc::new(Value::Closure(Rc::new(move |arg| {
                            let mut new_env = Env::new();
                            for (idx, val) in env_clone.iter() {
                                new_env.insert(idx + 1, val.clone());
                            }
                            new_env.insert(0, arg);
                            eval_trampolined(&body_clone, &new_env)
                        })));
                        state = State::Continue(closure);
                    }
                    Term::App(t1, t2) => {
                        stack.push(Frame::AppFun((*t2).clone(), env.clone()));
                        state = State::Compute((*t1).clone(), env);
                    }
                    Term::Int(n) => {
                        state = State::Continue(Rc::new(Value::Int(n)));
                    }
                    Term::Bool(b) => {
                        state = State::Continue(Rc::new(Value::Bool(b)));
                    }
                    Term::Add(t1, t2) => {
                        stack.push(Frame::AddLeft((*t2).clone(), env.clone()));
                        state = State::Compute((*t1).clone(), env);
                    }
                    Term::Sub(t1, t2) => {
                        stack.push(Frame::SubLeft((*t2).clone(), env.clone()));
                        state = State::Compute((*t1).clone(), env);
                    }
                    Term::Mul(t1, t2) => {
                        stack.push(Frame::MulLeft((*t2).clone(), env.clone()));
                        state = State::Compute((*t1).clone(), env);
                    }
                    Term::Eq(t1, t2) => {
                        stack.push(Frame::EqLeft((*t2).clone(), env.clone()));
                        state = State::Compute((*t1).clone(), env);
                    }
                    Term::Lt(t1, t2) => {
                        stack.push(Frame::LtLeft((*t2).clone(), env.clone()));
                        state = State::Compute((*t1).clone(), env);
                    }
                    Term::Gt(t1, t2) => {
                        stack.push(Frame::GtLeft((*t2).clone(), env.clone()));
                        state = State::Compute((*t1).clone(), env);
                    }
                    Term::If(cond, then_br, else_br) => {
                        stack.push(Frame::IfBranch((*then_br).clone(), (*else_br).clone(), env.clone()));
                        state = State::Compute((*cond).clone(), env);
                    }
                    Term::Fix(_, body) => {
                        let body_clone = (*body).clone();
                        let env_clone = env.clone();

                        let rec_closure: Rc<std::cell::RefCell<Option<Rc<Value>>>> =
                            Rc::new(std::cell::RefCell::new(None));
                        let rec_closure_ref = rec_closure.clone();

                        let mut new_fix_env = env_clone.clone();
                        for (idx, val) in env_clone.iter() {
                            new_fix_env.insert(idx + 1, val.clone());
                        }

                        let self_ref = Rc::new(Value::Closure(Rc::new(move |arg| {
                            let func: Rc<Value> = rec_closure_ref.borrow().clone().unwrap();
                            if let Value::Closure(f) = &*func {
                                f(arg)
                            } else {
                                panic!("Fixpoint should be a closure")
                            }
                        })));

                        new_fix_env.insert(0, self_ref);
                        let result = eval_trampolined(&body_clone, &new_fix_env);
                        *rec_closure.borrow_mut() = Some(result.clone());

                        state = State::Continue(result);
                    }
                }
            }
            State::Continue(value) => {
                match stack.pop() {
                    None => return value,
                    Some(Frame::AppFun(arg_term, arg_env)) => {
                        stack.push(Frame::AppArg(value));
                        state = State::Compute(arg_term, arg_env);
                    }
                    Some(Frame::AppArg(fun_val)) => {
                        match &*fun_val {
                            Value::Closure(f) => {
                                let result = f(value);
                                state = State::Continue(result);
                            }
                            _ => panic!("Runtime type error: expected closure"),
                        }
                    }
                    Some(Frame::AddLeft(t2, env2)) => {
                        stack.push(Frame::AddRight(value));
                        state = State::Compute(t2, env2);
                    }
                    Some(Frame::AddRight(v1)) => {
                        match (&*v1, &*value) {
                            (Value::Int(n1), Value::Int(n2)) => {
                                state = State::Continue(Rc::new(Value::Int(n1 + n2)));
                            }
                            _ => panic!("Addition on non-integers"),
                        }
                    }
                    Some(Frame::SubLeft(t2, env2)) => {
                        stack.push(Frame::SubRight(value));
                        state = State::Compute(t2, env2);
                    }
                    Some(Frame::SubRight(v1)) => {
                        match (&*v1, &*value) {
                            (Value::Int(n1), Value::Int(n2)) => {
                                state = State::Continue(Rc::new(Value::Int(n1 - n2)));
                            }
                            _ => panic!("Subtraction on non-integers"),
                        }
                    }
                    Some(Frame::MulLeft(t2, env2)) => {
                        stack.push(Frame::MulRight(value));
                        state = State::Compute(t2, env2);
                    }
                    Some(Frame::MulRight(v1)) => {
                        match (&*v1, &*value) {
                            (Value::Int(n1), Value::Int(n2)) => {
                                state = State::Continue(Rc::new(Value::Int(n1 * n2)));
                            }
                            _ => panic!("Multiplication on non-integers"),
                        }
                    }
                    Some(Frame::EqLeft(t2, env2)) => {
                        stack.push(Frame::EqRight(value));
                        state = State::Compute(t2, env2);
                    }
                    Some(Frame::EqRight(v1)) => {
                        match (&*v1, &*value) {
                            (Value::Int(n1), Value::Int(n2)) => {
                                state = State::Continue(Rc::new(Value::Bool(n1 == n2)));
                            }
                            (Value::Bool(b1), Value::Bool(b2)) => {
                                state = State::Continue(Rc::new(Value::Bool(b1 == b2)));
                            }
                            _ => panic!("Equality check on incompatible types"),
                        }
                    }
                    Some(Frame::LtLeft(t2, env2)) => {
                        stack.push(Frame::LtRight(value));
                        state = State::Compute(t2, env2);
                    }
                    Some(Frame::LtRight(v1)) => {
                        match (&*v1, &*value) {
                            (Value::Int(n1), Value::Int(n2)) => {
                                state = State::Continue(Rc::new(Value::Bool(n1 < n2)));
                            }
                            _ => panic!("Less than check on non-integers"),
                        }
                    }
                    Some(Frame::GtLeft(t2, env2)) => {
                        stack.push(Frame::GtRight(value));
                        state = State::Compute(t2, env2);
                    }
                    Some(Frame::GtRight(v1)) => {
                        match (&*v1, &*value) {
                            (Value::Int(n1), Value::Int(n2)) => {
                                state = State::Continue(Rc::new(Value::Bool(n1 > n2)));
                            }
                            _ => panic!("Greater than check on non-integers"),
                        }
                    }
                    Some(Frame::IfBranch(then_br, else_br, if_env)) => {
                        match &*value {
                            Value::Bool(true) => {
                                state = State::Compute(then_br, if_env);
                            }
                            Value::Bool(false) => {
                                state = State::Compute(else_br, if_env);
                            }
                            _ => panic!("If condition must be boolean"),
                        }
                    }
                }
            }
        }
    }
}

pub fn type_of(term: &Expr, ctx: &TypeContext) -> Result<Type, String> {
    match term {
        Expr::Var(name) => ctx
            .get(name)
            .cloned()
            .ok_or(format!("Unbound variable: {}", name)),

        Expr::Lam(param, param_type, body) => {
            let mut new_ctx = ctx.clone();
            new_ctx.insert(param.clone(), param_type.clone());
            let body_type = type_of(body, &new_ctx)?;
            Ok(Type::Arrow(Box::new(param_type.clone()), Box::new(body_type)))
        }

        Expr::App(t1, t2) => {
            let ty1 = type_of(t1, ctx)?;
            let ty2 = type_of(t2, ctx)?;
            match ty1 {
                Type::Arrow(arg_type, ret_type) => {
                    if *arg_type == ty2 {
                        Ok(*ret_type)
                    } else {
                        Err(format!("Type mismatch in application: expected {:?}, got {:?}", *arg_type, ty2))
                    }
                }
                _ => Err("Application of non-function type".to_string()),
            }
        }

        Expr::Int(_) => Ok(Type::Int),
        Expr::Bool(_) => Ok(Type::Bool),

        Expr::Add(t1, t2) => {
            let ty1 = type_of(t1, ctx)?;
            let ty2 = type_of(t2, ctx)?;
            if ty1 == Type::Int && ty2 == Type::Int {
                Ok(Type::Int)
            } else {
                Err("Addition requires integers".to_string())
            }
        }

        Expr::Sub(t1, t2) => {
            let ty1 = type_of(t1, ctx)?;
            let ty2 = type_of(t2, ctx)?;
            if ty1 == Type::Int && ty2 == Type::Int {
                Ok(Type::Int)
            } else {
                Err("Subtraction requires integers".to_string())
            }
        }

        Expr::Mul(t1, t2) => {
            let ty1 = type_of(t1, ctx)?;
            let ty2 = type_of(t2, ctx)?;
            if ty1 == Type::Int && ty2 == Type::Int {
                Ok(Type::Int)
            } else {
                Err("Multiplication requires integers".to_string())
            }
        }

        Expr::Eq(t1, t2) => {
            let ty1 = type_of(t1, ctx)?;
            let ty2 = type_of(t2, ctx)?;
            if ty1 == ty2 {
                Ok(Type::Bool)
            } else {
                Err("Equality check requires same type on both sides".to_string())
            }
        }

        Expr::Lt(t1, t2) => {
            let ty1 = type_of(t1, ctx)?;
            let ty2 = type_of(t2, ctx)?;
            if ty1 == Type::Int && ty2 == Type::Int {
                Ok(Type::Bool)
            } else {
                Err("Less than check requires integers".to_string())
            }
        }

        Expr::Gt(t1, t2) => {
            let ty1 = type_of(t1, ctx)?;
            let ty2 = type_of(t2, ctx)?;
            if ty1 == Type::Int && ty2 == Type::Int {
                Ok(Type::Bool)
            } else {
                Err("Greater than check requires integers".to_string())
            }
        }

        Expr::If(cond, then_branch, else_branch) => {
            let cond_type = type_of(cond, ctx)?;
            if cond_type != Type::Bool {
                return Err("If condition must be boolean".to_string());
            }

            let then_type = type_of(then_branch, ctx)?;
            let else_type = type_of(else_branch, ctx)?;

            if then_type == else_type {
                Ok(then_type)
            } else {
                Err("Branches of if must have the same type".to_string())
            }
        }

        Expr::Fix(name, annotated_type, body) => {
            let mut new_ctx = ctx.clone();
            new_ctx.insert(name.clone(), annotated_type.clone());
            let body_type = type_of(body, &new_ctx)?;

            // The body should have the same type as the annotated type
            if body_type == *annotated_type {
                Ok(annotated_type.clone())
            } else {
                Err(format!("Fixpoint type mismatch: expected {:?}, got {:?}", annotated_type, body_type))
            }
        }
    }
}

pub fn evaluate_named(term: &Expr) -> Rc<Value> {
    let mut converter = DeBruijnConverter::new();
    let term_db = converter.convert_term(term);
    let env = Env::new();
    eval(&term_db, &env)
}

pub fn evaluate_named_trampolined(term: &Expr) -> Rc<Value> {
    let mut converter = DeBruijnConverter::new();
    let term_db = converter.convert_term(term);
    let env = Env::new();
    eval_trampolined(&term_db, &env)
}

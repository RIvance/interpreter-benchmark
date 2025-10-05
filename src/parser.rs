use pest::Parser;
use pest_derive::Parser;
use crate::core::{Expr, Type};

#[derive(Parser)]
#[grammar = "stlc.pest"]
pub struct STLCParser;

pub fn parse(input: &str) -> Result<Expr, String> {
    let pairs = STLCParser::parse(Rule::program, input)
        .map_err(|e| format!("Parse error: {}", e))?;

    for pair in pairs {
        if pair.as_rule() == Rule::program {
            for inner_pair in pair.into_inner() {
                if inner_pair.as_rule() == Rule::expr {
                    return parse_expr(inner_pair);
                }
            }
        }
    }

    Err("No expression found".to_string())
}

fn parse_expr(pair: pest::iterators::Pair<Rule>) -> Result<Expr, String> {
    match pair.as_rule() {
        Rule::expr => {
            let inner = pair.into_inner().next().unwrap();
            parse_expr(inner)
        }
        Rule::lambda => parse_lambda(pair),
        Rule::fix => parse_fix(pair),
        Rule::if_expr => parse_if(pair),
        Rule::comparison => parse_comparison(pair),
        Rule::arith => parse_arith(pair),
        Rule::term => parse_term(pair),
        Rule::factor => parse_factor(pair),
        Rule::atom | Rule::var => {
            let inner = pair.into_inner().next().unwrap();
            parse_expr(inner)
        }
        Rule::ident => Ok(Expr::Var(pair.as_str().to_string())),
        Rule::integer => {
            let val = pair.as_str().parse::<i64>()
                .map_err(|e| format!("Invalid integer: {}", e))?;
            Ok(Expr::Int(val))
        }
        Rule::boolean => {
            let val = pair.as_str() == "true";
            Ok(Expr::Bool(val))
        }
        _ => Err(format!("Unexpected rule: {:?}", pair.as_rule()))
    }
}

fn parse_lambda(pair: pest::iterators::Pair<Rule>) -> Result<Expr, String> {
    let mut inner = pair.into_inner();
    let param = inner.next().unwrap().as_str().to_string();
    let param_type = parse_type(inner.next().unwrap())?;
    let body = parse_expr(inner.next().unwrap())?;

    Ok(Expr::Lam(param, param_type, Box::new(body)))
}

fn parse_fix(pair: pest::iterators::Pair<Rule>) -> Result<Expr, String> {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();
    let fix_type = parse_type(inner.next().unwrap())?;
    let body = parse_expr(inner.next().unwrap())?;

    Ok(Expr::Fix(name, fix_type, Box::new(body)))
}

fn parse_if(pair: pest::iterators::Pair<Rule>) -> Result<Expr, String> {
    let mut inner = pair.into_inner();
    let cond = parse_expr(inner.next().unwrap())?;
    let then_branch = parse_expr(inner.next().unwrap())?;
    let else_branch = parse_expr(inner.next().unwrap())?;

    Ok(Expr::If(Box::new(cond), Box::new(then_branch), Box::new(else_branch)))
}

fn parse_comparison(pair: pest::iterators::Pair<Rule>) -> Result<Expr, String> {
    let mut inner = pair.into_inner();
    let mut left = parse_expr(inner.next().unwrap())?;

    while let Some(op_pair) = inner.next() {
        if op_pair.as_rule() == Rule::comp_op {
            let op = op_pair.as_str();
            let right = parse_expr(inner.next().unwrap())?;
            left = match op {
                "==" => Expr::Eq(Box::new(left), Box::new(right)),
                "<" => Expr::Lt(Box::new(left), Box::new(right)),
                ">" => Expr::Gt(Box::new(left), Box::new(right)),
                _ => return Err(format!("Unknown comparison operator: {}", op))
            };
        } else {
            left = parse_expr(op_pair)?;
        }
    }

    Ok(left)
}

fn parse_arith(pair: pest::iterators::Pair<Rule>) -> Result<Expr, String> {
    let mut inner = pair.into_inner();
    let mut left = parse_expr(inner.next().unwrap())?;

    while let Some(op_pair) = inner.next() {
        if op_pair.as_rule() == Rule::add_op {
            let op = op_pair.as_str();
            let right = parse_expr(inner.next().unwrap())?;
            left = match op {
                "+" => Expr::Add(Box::new(left), Box::new(right)),
                "-" => Expr::Sub(Box::new(left), Box::new(right)),
                _ => return Err(format!("Unknown arithmetic operator: {}", op))
            };
        } else {
            left = parse_expr(op_pair)?;
        }
    }

    Ok(left)
}

fn parse_term(pair: pest::iterators::Pair<Rule>) -> Result<Expr, String> {
    let mut inner = pair.into_inner();
    let mut left = parse_expr(inner.next().unwrap())?;

    while let Some(op_pair) = inner.next() {
        if op_pair.as_rule() == Rule::mul_op {
            let op = op_pair.as_str();
            let right = parse_expr(inner.next().unwrap())?;
            left = match op {
                "*" => Expr::Mul(Box::new(left), Box::new(right)),
                _ => return Err(format!("Unknown multiplication operator: {}", op))
            };
        } else {
            left = parse_expr(op_pair)?;
        }
    }

    Ok(left)
}

fn parse_factor(pair: pest::iterators::Pair<Rule>) -> Result<Expr, String> {
    let mut inner = pair.into_inner();
    let mut left = parse_expr(inner.next().unwrap())?;

    // Handle function application
    for arg_pair in inner {
        let right = parse_expr(arg_pair)?;
        left = Expr::App(Box::new(left), Box::new(right));
    }

    Ok(left)
}

fn parse_type(pair: pest::iterators::Pair<Rule>) -> Result<Type, String> {
    match pair.as_rule() {
        Rule::type_expr => {
            let inner = pair.into_inner().next().unwrap();
            parse_type(inner)
        }
        Rule::type_arrow => {
            let mut inner = pair.into_inner();
            let mut left = parse_type(inner.next().unwrap())?;

            // Right-associative arrow types
            if let Some(right_pair) = inner.next() {
                let right = parse_type_arrow_rest(inner, right_pair)?;
                left = Type::Arrow(Box::new(left), Box::new(right));
            }

            Ok(left)
        }
        Rule::type_base => {
            match pair.as_str() {
                "Int" => Ok(Type::Int),
                "Bool" => Ok(Type::Bool),
                _ => Err(format!("Unknown type: {}", pair.as_str()))
            }
        }
        _ => Err(format!("Unexpected type rule: {:?}", pair.as_rule()))
    }
}

fn parse_type_arrow_rest(
    mut inner: pest::iterators::Pairs<Rule>,
    first: pest::iterators::Pair<Rule>
) -> Result<Type, String> {
    let mut left = parse_type(first)?;

    if let Some(right_pair) = inner.next() {
        let right = parse_type_arrow_rest(inner, right_pair)?;
        left = Type::Arrow(Box::new(left), Box::new(right));
    }

    Ok(left)
}

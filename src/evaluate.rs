use crate::tokens::{Binops, Expression, Primitives};

#[derive(Debug, PartialEq)]
pub enum EvalError {
    WrongParameterCount(String),
    WrongParameterType(String),
    NotYetImplemented,
}

impl Binops {
    fn apply(&self, parameters: Vec<Expression>) -> Result<Primitives, EvalError> {
        let mut primitives = Vec::with_capacity(parameters.len());
        for param in parameters {
            let prim = param.eval()?;
            primitives.push(prim);
        }

        if primitives.len() != 2 {
            return Err(EvalError::WrongParameterCount(format!(
                "Expected 2 parameters, found {}",
                primitives.len()
            )));
        }

        let f = primitives[0];
        let s = primitives[1];

        match self {
            Binops::Plus => match (f, s) {
                (Primitives::Int(a), Primitives::Int(b)) => Ok(Primitives::Int(a + b)),
                (Primitives::Float(a), Primitives::Float(b)) => Ok(Primitives::Float(a + b)),
                (Primitives::Float(f), Primitives::Int(i))
                | (Primitives::Int(i), Primitives::Float(f)) => Ok(Primitives::Float(i as f64 + f)),
                _ => Err(EvalError::WrongParameterType(
                    "Wrong types on input paramters".to_string(),
                )),
            },
            Binops::Mul => match (f, s) {
                (Primitives::Int(a), Primitives::Int(b)) => Ok(Primitives::Int(a * b)),
                (Primitives::Float(a), Primitives::Float(b)) => Ok(Primitives::Float(a * b)),
                (Primitives::Float(f), Primitives::Int(i))
                | (Primitives::Int(i), Primitives::Float(f)) => Ok(Primitives::Float(i as f64 * f)),
                _ => Err(EvalError::WrongParameterType(
                    "Wrong types on input paramters".to_string(),
                )),
            },
            Binops::Equals => Ok(Primitives::from(primitives[0] == primitives[1])),
            _ => Err(EvalError::NotYetImplemented),
        }
    }
}

impl Expression {
    pub fn eval(self) -> Result<Primitives, EvalError> {
        match self {
            Expression::Done(primitive) => Ok(primitive),
            Expression::Node(binary_operation, parameters) => binary_operation.apply(parameters),
        }
    }
}

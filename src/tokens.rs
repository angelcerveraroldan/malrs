#[derive(Debug, PartialEq, Clone, Copy)]
// Primitives
pub enum Primitives {
    Float(f64),
    Int(i64),

    True,
    False,
}

impl From<bool> for Primitives {
    fn from(value: bool) -> Self {
        if value {
            Primitives::True
        } else {
            Primitives::False
        }
    }
}
impl From<i64> for Primitives {
    fn from(value: i64) -> Self {
        Primitives::Int(value)
    }
}

impl From<f64> for Primitives {
    fn from(value: f64) -> Self {
        Primitives::Float(value)
    }
}

#[derive(Debug, PartialEq)]
// Binary operations
pub enum Binops {
    Plus,
    Minus,
    Mul,
    Div,
    Mod,

    Equals,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Done(Primitives),
    Node(Binops, Vec<Expression>),
}

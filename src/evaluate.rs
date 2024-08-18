use crate::parse::{Expression, ExpressionNode, NumberOperation, NumberTypes};

impl NumberTypes {
    pub fn equate(self, other: Self) -> (Self, Self) {
        match (&self, &other) {
            (NumberTypes::Integer(i), NumberTypes::Float(f))
            | (NumberTypes::Float(f), NumberTypes::Integer(i)) => {
                (NumberTypes::Float(*i as f64), NumberTypes::Float(*f))
            }
            _ => (self, other),
        }
    }

    pub fn common_type(cs: Vec<NumberTypes>) -> Vec<NumberTypes> {
        if let None = cs.iter().find(|x| match x {
            NumberTypes::Integer(_) => false,
            NumberTypes::Float(_) => true,
        }) {
            return cs;
        }

        cs.into_iter()
            .map(|x| match x {
                NumberTypes::Integer(i) => NumberTypes::Float(i as f64),
                o => o,
            })
            .collect()
    }
}

impl ExpressionNode {
    fn eval_add(&self) -> NumberTypes {
        if self.children_count != 2 {
            panic!("+ expects EXACTLY 2 children")
        }

        let a = self.children[0].evaluate();
        let b = self.children[1].evaluate();
        match a.equate(b) {
            (NumberTypes::Integer(i1), NumberTypes::Integer(i2)) => NumberTypes::from(i1 + i2),
            (NumberTypes::Float(f1), NumberTypes::Float(f2)) => NumberTypes::from(f1 + f2),
            _ => unreachable!("Types have been equated"),
        }
    }
    fn eval_sub(&self) -> NumberTypes {
        if self.children_count != 2 {
            panic!("+ expects EXACTLY 2 children")
        }

        let a = self.children[0].evaluate();
        let b = self.children[1].evaluate();
        match a.equate(b) {
            (NumberTypes::Integer(i1), NumberTypes::Integer(i2)) => NumberTypes::from(i1 - i2),
            (NumberTypes::Float(f1), NumberTypes::Float(f2)) => NumberTypes::from(f1 - f2),
            _ => unreachable!("Types have been equated"),
        }
    }

    fn eval_mul(&self) -> NumberTypes {
        if self.children_count != 2 {
            panic!("+ expects EXACTLY 2 children")
        }

        let a = self.children[0].evaluate();
        let b = self.children[1].evaluate();
        match a.equate(b) {
            (NumberTypes::Integer(i1), NumberTypes::Integer(i2)) => NumberTypes::from(i1 * i2),
            (NumberTypes::Float(f1), NumberTypes::Float(f2)) => NumberTypes::from(f1 * f2),
            _ => unreachable!("Types have been equated"),
        }
    }

    fn eval_div(&self) -> NumberTypes {
        if self.children_count != 2 {
            panic!("+ expects EXACTLY 2 children")
        }

        let a = self.children[0].evaluate();
        let b = self.children[1].evaluate();
        match a.equate(b) {
            (NumberTypes::Integer(i1), NumberTypes::Integer(i2)) => {
                NumberTypes::from(i1 as f64 / i2 as f64)
            }
            (NumberTypes::Float(f1), NumberTypes::Float(f2)) => NumberTypes::from(f1 / f2),
            _ => unreachable!("Types have been equated"),
        }
    }

    fn eval_max(&self) -> NumberTypes {
        self.children
            .iter()
            .map(|c| c.evaluate())
            .max_by(|a, b| match a.clone().equate(b.clone()) {
                (NumberTypes::Integer(i1), NumberTypes::Integer(i2)) => i1.cmp(&i2),
                (NumberTypes::Float(f1), NumberTypes::Float(f2)) => match f1.partial_cmp(&f2) {
                    None => panic!("Not a number?"),
                    Some(x) => x,
                },
                _ => unreachable!("Types have been equated"),
            })
            .unwrap()
    }

    fn eval_min(&self) -> NumberTypes {
        let numbers = self.children.iter().map(|c| c.evaluate()).collect();

        NumberTypes::common_type(numbers)
            .into_iter()
            .min_by(|a, b| match a.clone().equate(b.clone()) {
                (NumberTypes::Integer(i1), NumberTypes::Integer(i2)) => i1.cmp(&i2),
                (NumberTypes::Float(f1), NumberTypes::Float(f2)) => match f1.partial_cmp(&f2) {
                    None => panic!("Not a number?"),
                    Some(x) => x,
                },
                _ => unreachable!("Types have been equated"),
            })
            .unwrap()
    }

    pub fn evaluate(&self) -> NumberTypes {
        use crate::parse::NumberOperation::*;
        match self.operator {
            Add => self.eval_add(),
            Sub => self.eval_sub(),
            Mul => self.eval_mul(),
            Div => self.eval_div(),

            Min => self.eval_min(),
            Max => self.eval_max(),
        }
    }
}

impl Expression {
    pub fn evaluate(&self) -> NumberTypes {
        match self {
            Expression::Bottom(num) => num.clone(),
            Expression::Node(node) => node.evaluate(),
        }
    }
}

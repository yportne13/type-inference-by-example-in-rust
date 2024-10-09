use std::collections::HashMap;

/////////////////////////////////
// Syntax tree
/////////////////////////////////

#[derive(Debug, Clone)]
pub enum Expression {
    Lambda(String, Box<Expression>),
    Apply(Box<Expression>, Box<Expression>),
    Variable(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Constructor(String, Vec<Type>),
    Variable(usize),
}

#[derive(Debug, Clone)]
pub enum Constraint {
    Equality(Type, Type),
}

/////////////////////////////////
// Type inference
/////////////////////////////////

#[derive(Debug)]
pub struct TypeError {
    message: String,
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for TypeError {}

pub struct Inference {
    type_constraints: Vec<Constraint>,
    substitution: Vec<Type>,
}

impl Inference {
    pub fn new() -> Self {
        Inference {
            type_constraints: Vec::new(),
            substitution: Vec::new(),
        }
    }

    fn fresh_type_variable(&mut self) -> Type {
        let result = Type::Variable(self.substitution.len());
        self.substitution.push(result.clone());
        result
    }

    fn infer_type(
        &mut self,
        expression: &Expression,
        environment: &HashMap<String, Type>,
    ) -> Type {
        match expression {
            Expression::Lambda(x, e) => {
                let t1 = self.fresh_type_variable();
                let mut environment2 = environment.clone();
                environment2.insert(x.clone(), t1.clone());
                let t2 = self.infer_type(e, &environment2);
                Type::Constructor("Function1".to_string(), vec![t1, t2])
            }
            Expression::Apply(e1, e2) => {
                let t1 = self.infer_type(e1, environment);
                let t2 = self.infer_type(e2, environment);
                let t3 = self.fresh_type_variable();
                self.type_constraints
                    .push(Constraint::Equality(t1, Type::Constructor("Function1".to_string(), vec![t2, t3.clone()])));
                t3
            }
            Expression::Variable(x) => environment.get(x).cloned().unwrap_or_else(|| {
                panic!("Variable not in scope: {}", x)
            }),
        }
    }

    fn solve_constraints(&mut self) {
        let constraint = self.type_constraints.drain(..).collect::<Vec<_>>();
        for constraint in constraint {
            if let Constraint::Equality(t1, t2) = constraint {
                self.unify(t1, t2);
            }
        }
    }

    fn unify(&mut self, t1: Type, t2: Type) {
        match (t1, t2) {
            (Type::Variable(i), t2) if self.substitution[i] != Type::Variable(i) => {
                self.unify(self.substitution[i].clone(), t2);
            }
            (t1, Type::Variable(i)) if self.substitution[i] != Type::Variable(i) => {
                self.unify(t1, self.substitution[i].clone());
            }
            (Type::Variable(i), t2) => {
                if self.occurs_in(i, &t2) {
                    panic!("Infinite type: ${} = {:?}", i, t2);
                }
                self.substitution[i] = t2;
            }
            (t1, Type::Variable(i)) => {
                if self.occurs_in(i, &t1) {
                    panic!("Infinite type: ${} = {:?}", i, t1);
                }
                self.substitution[i] = t1;
            }
            (Type::Constructor(name1, generics1), Type::Constructor(name2, generics2)) if name1 == name2 => {
                if generics1.len() != generics2.len() {
                    panic!("Generics mismatch: {:?} vs. {:?}", generics1, generics2);
                }
                for (t1, t2) in generics1.iter().zip(generics2.iter()) {
                    self.unify(t1.clone(), t2.clone());
                }
            }
            (t1, t2) => {
                panic!("Type mismatch: {:?} vs. {:?}", t1, t2);
            }
        }
    }

    fn occurs_in(&self, index: usize, t: &Type) -> bool {
        match t {
            Type::Variable(i) if self.substitution[*i] != Type::Variable(*i) => {
                self.occurs_in(index, &self.substitution[*i])
            }
            Type::Variable(i) => *i == index,
            Type::Constructor(_, generics) => generics.iter().any(|t| self.occurs_in(index, t)),
        }
    }

    fn substitute(&self, t: &Type) -> Type {
        match t {
            Type::Variable(i) if self.substitution[*i] != Type::Variable(*i) => {
                self.substitute(&self.substitution[*i])
            }
            Type::Constructor(name, generics) => {
                Type::Constructor(name.clone(), generics.iter().map(|t| self.substitute(t)).collect())
            }
            _ => t.clone(),
        }
    }
}

/////////////////////////////////
// Tests
/////////////////////////////////

fn initial_environment() -> HashMap<String, Type> {
    let mut env = HashMap::new();
    for op in ["+", "-", "*", "/"].iter() {
        env.insert(
            op.to_string(),
            Type::Constructor(
                "Function1".to_string(),
                vec![
                    Type::Constructor("Int".to_string(), vec![]),
                    Type::Constructor(
                        "Function1".to_string(),
                        vec![
                            Type::Constructor("Int".to_string(), vec![]),
                            Type::Constructor("Int".to_string(), vec![]),
                        ],
                    ),
                ],
            ),
        );
    }
    for i in 0..100 {
        env.insert(i.to_string(), Type::Constructor("Int".to_string(), vec![]));
    }
    env
}

fn infer(expression: Expression) -> Result<Type, TypeError> {
    let mut inference = Inference::new();
    let t = inference.infer_type(&expression, &initial_environment());
    inference.solve_constraints();
    Ok(inference.substitute(&t))
}

fn print_infer(expression: Expression) -> String {
    match infer(expression) {
        Ok(t) => format!("{:?}", t),
        Err(e) => e.message,
    }
}

#[test]
fn test() {
    println!(
        "{}",
        print_infer(Expression::Lambda(
            "x".to_string(),
            Box::new(Expression::Apply(
                Box::new(Expression::Apply(
                    Box::new(Expression::Variable("+".to_string())),
                    Box::new(Expression::Variable("x".to_string())),
                )),
                Box::new(Expression::Variable("x".to_string())),
            )),
        ))
    );
}
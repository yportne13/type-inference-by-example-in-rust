use std::collections::HashMap;

/////////////////////////////////
// Syntax tree
/////////////////////////////////

#[derive(Debug, Clone)]
pub enum Expression {
    Lambda(Vec<Parameter>, Option<Type>, Box<Expression>),
    Apply(Box<Expression>, Vec<Expression>),
    Variable(String),
    Let(String, Option<Type>, Box<Expression>, Box<Expression>),
    Int(i32),
    String(String),
    Array(Option<Type>, Vec<Expression>),
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: String,
    pub type_annotation: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Constructor(String, Vec<Type>),
    Variable(usize),
}

impl Type {
    pub fn to_string(&self) -> String {
        match self {
            Type::Constructor(name, generics) => {
                if generics.is_empty() {
                    name.clone()
                } else {
                    format!("{name}<{}>", generics.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", "))
                }
            }
            Type::Variable(index) => format!("${index}"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Constraint {
    CEquality(Type, Type),
}

/////////////////////////////////
// Type inference
/////////////////////////////////

#[derive(Debug)]
pub struct TypeError {
    pub message: String,
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
        Self {
            type_constraints: Vec::new(),
            substitution: Vec::new(),
        }
    }

    pub fn fresh_type_variable(&mut self) -> Type {
        let result = Type::Variable(self.substitution.len());
        self.substitution.push(result.clone());
        result
    }

    pub fn infer(
        &mut self,
        environment: HashMap<String, Type>,
        expected_type: Type,
        expression: Expression,
    ) -> Result<Expression, String> {
        match expression {
            Expression::Lambda(parameters, return_type, body) => {
                let param_len = parameters.len();
                let new_return_type = return_type.unwrap_or_else(|| self.fresh_type_variable());
                let new_parameter_types: Vec<Type> = parameters
                    .iter()
                    .map(|p| p.type_annotation.clone().unwrap_or_else(|| self.fresh_type_variable()))
                    .collect();
                let new_parameters: Vec<Parameter> = parameters
                    .into_iter()
                    .zip(new_parameter_types.clone())
                    .map(|(p, t)| Parameter {
                        name: p.name,
                        type_annotation: Some(t),
                    })
                    .collect();
                let new_environment: HashMap<String, Type> = environment
                    .into_iter()
                    .chain(
                        new_parameters
                            .iter()
                            .map(|p| (p.name.clone(), p.type_annotation.clone().unwrap())),
                    )
                    .collect();
                let new_body = self.infer(new_environment, new_return_type.clone(), *body)?;
                self.type_constraints.push(Constraint::CEquality(
                    expected_type,
                    Type::Constructor(
                        format!("Function{}", param_len),
                        new_parameter_types.into_iter().chain(vec![new_return_type.clone()]).collect(),
                    ),
                ));
                Ok(Expression::Lambda(new_parameters, Some(new_return_type), Box::new(new_body)))
            }
            Expression::Apply(function, arguments) => {
                let argument_types: Vec<Type> = arguments.iter().map(|_| self.fresh_type_variable()).collect();
                let function_type = Type::Constructor(
                    format!("Function{}", arguments.len()),
                    argument_types.clone().into_iter().chain(vec![expected_type]).collect(),
                );
                let new_function = self.infer(environment.clone(), function_type, *function)?;
                let new_arguments: Vec<Expression> = arguments
                    .into_iter()
                    .zip(argument_types)
                    .map(|(argument, t)| self.infer(environment.clone(), t, argument))
                    .collect::<Result<Vec<_>, String>>()?;
                Ok(Expression::Apply(Box::new(new_function), new_arguments))
            }
            Expression::Variable(name) => {
                let variable_type = environment.get(&name).cloned().ok_or({
                    format!("Variable not in scope: {}", name)
                })?;
                self.type_constraints.push(Constraint::CEquality(expected_type, variable_type.clone()));
                Ok(Expression::Variable(name))
            }
            Expression::Let(name, type_annotation, value, body) => {
                let new_type_annotation = type_annotation.unwrap_or_else(|| self.fresh_type_variable());
                let new_value = self.infer(environment.clone(), new_type_annotation.clone(), *value)?;
                let new_environment = environment
                    .into_iter()
                    .chain(vec![(name.clone(), new_type_annotation.clone())])
                    .collect();
                let new_body = self.infer(new_environment, expected_type, *body)?;
                Ok(Expression::Let(name, Some(new_type_annotation), Box::new(new_value), Box::new(new_body)))
            }
            Expression::Int(_) => {
                self.type_constraints.push(Constraint::CEquality(expected_type, Type::Constructor("Int".to_string(), vec![])));
                Ok(expression)
            }
            Expression::String(_) => {
                self.type_constraints.push(Constraint::CEquality(expected_type, Type::Constructor("String".to_string(), vec![])));
                Ok(expression)
            }
            Expression::Array(item_type, items) => {
                let new_item_type = item_type.unwrap_or_else(|| self.fresh_type_variable());
                let new_items: Vec<Expression> = items
                    .into_iter()
                    .map(|item| self.infer(environment.clone(), new_item_type.clone(), item))
                    .collect::<Result<Vec<_>, String>>()?;
                self.type_constraints.push(Constraint::CEquality(
                    expected_type,
                    Type::Constructor("Array".to_string(), vec![new_item_type.clone()]),
                ));
                Ok(Expression::Array(Some(new_item_type), new_items))
            }
        }
    }

    pub fn solve_constraints(&mut self) {
        let constraint = self.type_constraints.drain(..).collect::<Vec<_>>();
        for constraint in constraint {
            if let Constraint::CEquality(t1, t2) = constraint {
                self.unify(t1, t2);
            }
        }
    }

    pub fn unify(&mut self, t1: Type, t2: Type) -> Result<(), String> {
        match (t1, t2) {
            (Type::Variable(i1), Type::Variable(i2)) if i1 == i2 => {}
            (Type::Variable(i), t2) if self.substitution[i] != Type::Variable(i) => {
                self.unify(self.substitution[i].clone(), t2)?
            }
            (t1, Type::Variable(i)) if self.substitution[i] != Type::Variable(i) => {
                self.unify(t1, self.substitution[i].clone())?
            }
            (Type::Variable(i), t2) => {
                if self.occurs_in(i, &t2) {
                    return Err(format!("Infinite type: ${} = {:?}", i, self.substitute(&t2)));
                }
                self.substitution[i] = t2;
            }
            (t1, Type::Variable(i)) => {
                if self.occurs_in(i, &t1) {
                    return Err(format!("Infinite type: ${} = {:?}", i, self.substitute(&t1)));
                }
                self.substitution[i] = t1;
            }
            (Type::Constructor(name1, generics1), Type::Constructor(name2, generics2)) => {
                if name1 != name2 || generics1.len() != generics2.len() {
                    return Err(format!(
                        "Type mismatch: {:?} vs. {:?}",
                        self.substitute(&Type::Constructor(name1, generics1)),
                        self.substitute(&Type::Constructor(name2, generics2))
                    ));
                }
                for (t1, t2) in generics1.into_iter().zip(generics2) {
                    self.unify(t1, t2)?
                }
            }
        }
        Ok(())
    }

    pub fn occurs_in(&self, index: usize, t: &Type) -> bool {
        match t {
            Type::Variable(i) if self.substitution[*i] != Type::Variable(*i) => {
                self.occurs_in(index, &self.substitution[*i])
            }
            Type::Variable(i) => *i == index,
            Type::Constructor(_, generics) => generics.iter().any(|t| self.occurs_in(index, t)),
        }
    }

    pub fn substitute(&self, t: &Type) -> Type {
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

    pub fn substitute_expression(&self, expression: Expression) -> Expression {
        match expression {
            Expression::Lambda(parameters, return_type, body) => {
                let new_return_type = return_type.map(|t| self.substitute(&t));
                let new_parameters: Vec<Parameter> = parameters
                    .into_iter()
                    .map(|p| Parameter {
                        name: p.name,
                        type_annotation: p.type_annotation.map(|t| self.substitute(&t)),
                    })
                    .collect();
                let new_body = self.substitute_expression(*body);
                Expression::Lambda(new_parameters, new_return_type, Box::new(new_body))
            }
            Expression::Apply(function, arguments) => {
                let new_function = self.substitute_expression(*function);
                let new_arguments: Vec<Expression> = arguments
                    .into_iter()
                    .map(|arg| self.substitute_expression(arg))
                    .collect();
                Expression::Apply(Box::new(new_function), new_arguments)
            }
            Expression::Variable(_) => expression,
            Expression::Let(name, type_annotation, value, body) => {
                let new_type_annotation = type_annotation.map(|t| self.substitute(&t));
                let new_value = self.substitute_expression(*value);
                let new_body = self.substitute_expression(*body);
                Expression::Let(name, new_type_annotation, Box::new(new_value), Box::new(new_body))
            }
            Expression::Int(_) => expression,
            Expression::String(_) => expression,
            Expression::Array(item_type, items) => {
                let new_item_type = item_type.map(|t| self.substitute(&t));
                let new_items: Vec<Expression> = items
                    .into_iter()
                    .map(|item| self.substitute_expression(item))
                    .collect();
                Expression::Array(new_item_type, new_items)
            }
        }
    }
}

/////////////////////////////////
// Tests
/////////////////////////////////

fn initial_environment() -> HashMap<String, Type> {
    vec![
        "+", "-", "*", "/",
    ]
    .into_iter()
    .map(|op| (
        op.to_string(),
        Type::Constructor(
            "Function2".to_string(),
            vec![
                Type::Constructor("Int".to_string(), vec![]),
                Type::Constructor("Int".to_string(), vec![]),
                Type::Constructor("Int".to_string(), vec![]),
            ],
        ),
    ))
    .collect()
}

fn infer(expression: Expression) -> Result<Expression, String> {
    let mut inference = Inference::new();
    let expect = inference.fresh_type_variable();
    let new_expression = inference.infer(HashMap::new(), expect, expression)?;
    inference.solve_constraints();
    Ok(inference.substitute_expression(new_expression))
}

pub fn print_infer(expression: Expression) -> String {
    let e = infer(expression);
    format!("{:?}", e)
}

#[test]
fn test() {
    let result = print_infer(Expression::Let(
        "singleton".to_string(),
        None,
        Box::new(Expression::Lambda(
            vec![Parameter {
                name: "x".to_string(),
                type_annotation: None,
            }],
            None,
            Box::new(Expression::Array(
                None,
                vec![Expression::Variable("x".to_string())],
            )),
        )),
        Box::new(Expression::Apply(
            Box::new(Expression::Variable("singleton".to_string())),
            vec![Expression::Int(42)],
        )),
    ));
    println!("{}", result);
}
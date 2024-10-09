use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;

/////////////////////////////////
// Syntax tree
/////////////////////////////////

#[derive(Debug, Clone)]
pub enum Expression {
    Functions(Vec<GenericFunction>, Box<Expression>),
    Lambda(Vec<Parameter>, Option<Type>, Box<Expression>),
    Apply(Box<Expression>, Vec<Expression>),
    Variable(String, Vec<Type>),
    Let(String, Option<Type>, Box<Expression>, Box<Expression>),
    Int(i32),
    String(String),
    Array(Option<Type>, Vec<Expression>),
    Semicolon(Box<Expression>, Box<Expression>),
}

#[derive(Debug, Clone)]
pub struct GenericFunction {
    pub name: String,
    pub type_annotation: Option<GenericType>,
    pub lambda: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct GenericType {
    pub generics: Vec<String>,
    pub uninstantiated_type: Type,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: String,
    pub type_annotation: Option<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Constructor(String, Vec<Type>),
    Variable(usize),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Constructor(name, generics) => {
                if generics.is_empty() {
                    write!(f, "{}", name)
                } else {
                    write!(f, "{}[{}]", name, generics.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", "))
                }
            }
            Type::Variable(index) => write!(f, "${}", index),
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

struct GenericName {
    char: char,
    idx: u32,
}

impl GenericName {
    pub fn new() -> Self {
        Self {
            char: 'A',
            idx: 0,
        }
    }
    pub fn next(&mut self) -> String {
        let ret = format!("{}{}", self.char, self.idx);
        if self.char == 'Z' {
            self.char = 'A';
            self.idx += 1;
        } else {
            self.char = std::char::from_u32(self.char as u32 + 1).unwrap();
        }
        ret
    }
}

pub struct Inference {
    type_constraints: Vec<Constraint>,
    substitution: Vec<Type>,
    generic_parameter_names: GenericName,
}

impl Inference {
    pub fn new() -> Self {
        Self {
            type_constraints: Vec::new(),
            substitution: Vec::new(),
            generic_parameter_names: GenericName::new(),
        }
    }

    fn fresh_type_variable(&mut self) -> Type {
        let result = Type::Variable(self.substitution.len());
        self.substitution.push(result.clone());
        result
    }

    pub fn infer(
        &mut self,
        environment: HashMap<String, GenericType>,
        expected_type: Type,
        expression: Expression,
    ) -> Result<Expression, TypeError> {
        match expression {
            Expression::Functions(functions, body) => {
                let recursive_environment: HashMap<_, _> = environment
                    .into_iter()
                    .chain(functions.iter().map(|function| {
                        (
                            function.name.clone(),
                            function
                                .type_annotation
                                .clone()
                                .unwrap_or(GenericType {
                                    generics: Vec::new(),
                                    uninstantiated_type: self.fresh_type_variable(),
                                }),
                        )
                    }))
                    .collect();
                let ungeneralized_functions = functions
                    .into_iter()
                    .map(|function| {
                        let uninstantiated_type = recursive_environment[&function.name].uninstantiated_type.clone();
                        let lambda = self.infer(recursive_environment.clone(), uninstantiated_type, *function.lambda)?;
                        Ok(GenericFunction {
                            name: function.name,
                            type_annotation: function.type_annotation,
                            lambda: Box::new(lambda),
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                self.solve_constraints()?;
                let new_functions = ungeneralized_functions
                    .into_iter()
                    .map(|function| {
                        if function.type_annotation.is_some() {
                            Ok(function)
                        } else {
                            let function_type = recursive_environment[&function.name].uninstantiated_type.clone();
                            let (new_type_annotation, new_lambda) = self.generalize(recursive_environment.clone(), function_type, *function.lambda)?;
                            Ok(GenericFunction {
                                name: function.name,
                                type_annotation: Some(new_type_annotation),
                                lambda: Box::new(new_lambda),
                            })
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let new_environment = recursive_environment
                    .into_iter()
                    .chain(new_functions.iter().map(|function| {
                        (
                            function.name.clone(),
                            function.type_annotation.clone().unwrap(),
                        )
                    }))
                    .collect();
                let new_body = self.infer(new_environment, expected_type, *body)?;
                Ok(Expression::Functions(new_functions, Box::new(new_body)))
            }
            Expression::Lambda(parameters, return_type, body) => {
                let param_len = parameters.len();
                let new_return_type = return_type.unwrap_or_else(|| self.fresh_type_variable());
                let new_parameter_types = parameters
                    .iter()
                    .map(|p| p.type_annotation.clone().unwrap_or_else(|| self.fresh_type_variable()))
                    .collect::<Vec<_>>();
                let new_parameters = parameters
                    .into_iter()
                    .zip(new_parameter_types.clone())
                    .map(|(p, t)| Parameter {
                        name: p.name,
                        type_annotation: Some(t),
                    })
                    .collect::<Vec<_>>();
                let new_environment = environment
                    .into_iter()
                    .chain(new_parameters.iter().map(|p| {
                        (
                            p.name.clone(),
                            GenericType {
                                generics: Vec::new(),
                                uninstantiated_type: p.type_annotation.clone().unwrap(),
                            },
                        )
                    }))
                    .collect();
                let new_body = self.infer(new_environment, new_return_type.clone(), *body)?;
                self.type_constraints.push(Constraint::CEquality(
                    expected_type,
                    Type::Constructor(
                        format!("Function{}", param_len),
                        new_parameter_types.into_iter().chain(std::iter::once(new_return_type.clone())).collect(),
                    ),
                ));
                Ok(Expression::Lambda(new_parameters, Some(new_return_type), Box::new(new_body)))
            }
            Expression::Apply(function, arguments) => {
                let argument_types = arguments.iter().map(|_| self.fresh_type_variable()).collect::<Vec<_>>();
                let function_type = Type::Constructor(
                    format!("Function{}", arguments.len()),
                    argument_types.clone().into_iter().chain(std::iter::once(expected_type.clone())).collect(),
                );
                let new_function = self.infer(environment.clone(), function_type, *function)?;
                let new_arguments = arguments
                    .into_iter()
                    .zip(argument_types)
                    .map(|(argument, t)| self.infer(environment.clone(), t, argument))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Expression::Apply(Box::new(new_function), new_arguments))
            }
            Expression::Variable(name, generics) => {
                let generic_type = environment.get(&name).cloned().ok_or_else(|| TypeError {
                    message: format!("Variable not in scope: {}", name),
                })?;
                let new_generics = generic_type.generics.iter().map(|_| self.fresh_type_variable()).collect::<Vec<_>>();
                let instantiation = generic_type
                    .generics
                    .iter()
                    .cloned()
                    .zip(new_generics.clone())
                    .collect::<HashMap<_, _>>();
                let variable_type = self.instantiate(instantiation, generic_type.uninstantiated_type.clone());
                if !generics.is_empty() {
                    if generics.len() != generic_type.generics.len() {
                        return Err(TypeError {
                            message: format!("Wrong number of generics: {:?} vs. {:?}", Expression::Variable(name, generics), generic_type),
                        });
                    }
                    for (t, v) in generics.into_iter().zip(new_generics.clone()) {
                        self.type_constraints.push(Constraint::CEquality(t, v));
                    }
                }
                self.type_constraints.push(Constraint::CEquality(expected_type, variable_type.clone()));
                Ok(Expression::Variable(name, new_generics))
            }
            Expression::Let(name, type_annotation, value, body) => {
                let new_type_annotation = type_annotation.unwrap_or_else(|| self.fresh_type_variable());
                let new_value = self.infer(environment.clone(), new_type_annotation.clone(), *value)?;
                let new_environment = environment
                    .into_iter()
                    .chain(std::iter::once((
                        name.clone(),
                        GenericType {
                            generics: Vec::new(),
                            uninstantiated_type: new_type_annotation.clone(),
                        },
                    )))
                    .collect();
                let new_body = self.infer(new_environment, expected_type, *body)?;
                Ok(Expression::Let(name, Some(new_type_annotation), Box::new(new_value), Box::new(new_body)))
            }
            Expression::Int(_) => {
                self.type_constraints.push(Constraint::CEquality(expected_type, Type::Constructor("Int".to_string(), Vec::new())));
                Ok(expression)
            }
            Expression::String(_) => {
                self.type_constraints.push(Constraint::CEquality(expected_type, Type::Constructor("String".to_string(), Vec::new())));
                Ok(expression)
            }
            Expression::Array(item_type, items) => {
                let new_item_type = item_type.unwrap_or_else(|| self.fresh_type_variable());
                let new_items = items
                    .into_iter()
                    .map(|item| self.infer(environment.clone(), new_item_type.clone(), item))
                    .collect::<Result<Vec<_>, _>>()?;
                self.type_constraints.push(Constraint::CEquality(
                    expected_type,
                    Type::Constructor("Array".to_string(), vec![new_item_type.clone()]),
                ));
                Ok(Expression::Array(Some(new_item_type), new_items))
            }
            Expression::Semicolon(before, after) => {
                let expect = self.fresh_type_variable();
                let new_before = self.infer(environment.clone(), expect, *before)?;
                let new_after = self.infer(environment, expected_type, *after)?;
                Ok(Expression::Semicolon(Box::new(new_before), Box::new(new_after)))
            }
        }
    }

    fn solve_constraints(&mut self) -> Result<(), TypeError> {
        let constraint = self.type_constraints.drain(..).collect::<Vec<_>>();
        for constraint in constraint {
            match constraint {
                Constraint::CEquality(t1, t2) => self.unify(t1, t2)?,
            }
        }
        Ok(())
    }

    fn unify(&mut self, t1: Type, t2: Type) -> Result<(), TypeError> {
        match (t1, t2) {
            (Type::Variable(i1), Type::Variable(i2)) if i1 == i2 => Ok(()),
            (Type::Variable(i), t2) if self.substitution[i] != Type::Variable(i) => self.unify(self.substitution[i].clone(), t2),
            (t1, Type::Variable(i)) if self.substitution[i] != Type::Variable(i) => self.unify(t1, self.substitution[i].clone()),
            (Type::Variable(i), t) => {
                if self.occurs_in(i, &t) {
                    return Err(TypeError {
                        message: format!("Infinite type: ${} = {}", i, self.substitute(&self.substitution, &t)),
                    });
                }
                self.substitution[i] = t;
                Ok(())
            }
            (t, Type::Variable(i)) => {
                if self.occurs_in(i, &t) {
                    return Err(TypeError {
                        message: format!("Infinite type: ${} = {}", i, self.substitute(&self.substitution, &t)),
                    });
                }
                self.substitution[i] = t;
                Ok(())
            }
            (Type::Constructor(name1, generics1), Type::Constructor(name2, generics2)) => {
                if name1 != name2 || generics1.len() != generics2.len() {
                    return Err(TypeError {
                        message: format!(
                            "Type mismatch: {} vs. {}",
                            self.substitute(&self.substitution, &Type::Constructor(name1, generics1)),
                            self.substitute(&self.substitution, &Type::Constructor(name2, generics2))
                        ),
                    });
                }
                for (t1, t2) in generics1.into_iter().zip(generics2) {
                    self.unify(t1, t2)?;
                }
                Ok(())
            }
        }
    }

    fn occurs_in(&self, index: usize, t: &Type) -> bool {
        match t {
            Type::Variable(i) if self.substitution[*i] != Type::Variable(*i) => self.occurs_in(index, &self.substitution[*i]),
            Type::Variable(i) => *i == index,
            Type::Constructor(_, generics) => generics.iter().any(|t| self.occurs_in(index, t)),
        }
    }

    fn free_in_type(&self, t: &Type) -> HashSet<usize> {
        match t {
            Type::Variable(i) if self.substitution[*i] != Type::Variable(*i) => self.free_in_type(&self.substitution[*i]),
            Type::Variable(i) => HashSet::from_iter(std::iter::once(*i)),
            Type::Constructor(_, generics) => generics
                .iter()
                .flat_map(|t| self.free_in_type(t))
                .collect(),
        }
    }

    fn free_in_generic_type(&self, t: &GenericType) -> HashSet<usize> {
        self.free_in_type(&t.uninstantiated_type)
    }

    fn free_in_environment(&self, environment: &HashMap<String, GenericType>) -> HashSet<usize> {
        environment
            .values()
            .flat_map(|t| self.free_in_generic_type(t))
            .collect()
    }

    fn generalize(
        &mut self,
        environment: HashMap<String, GenericType>,
        t: Type,
        expression: Expression,
    ) -> Result<(GenericType, Expression), TypeError> {
        let generic_type_variables = self.free_in_type(&t)
            .difference(&self.free_in_environment(&environment))
            .cloned()
            .collect::<HashSet<_>>();
        let generic_names = generic_type_variables
            .into_iter()
            .map(|i| (i, self.generic_parameter_names.next()))
            .collect::<Vec<_>>();
        let mut local_substitution = self.substitution.clone();
        for (i, name) in &generic_names {
            local_substitution[*i] = Type::Constructor(name.clone(), Vec::new());
        }
        let new_expression = self.substitute_expression(&local_substitution, expression)?;
        let new_type = self.substitute(&local_substitution, &t);
        Ok((
            GenericType {
                generics: generic_names.into_iter().map(|(_, name)| name).collect(),
                uninstantiated_type: new_type,
            },
            new_expression,
        ))
    }

    fn instantiate(&self, instantiation: HashMap<String, Type>, t: Type) -> Type {
        match t {
            Type::Variable(i) if self.substitution[i] != Type::Variable(i) => self.instantiate(instantiation, self.substitution[i].clone()),
            Type::Constructor(name, generics) => {
                if let Some(instantiation_type) = instantiation.get(&name) {
                    if !generics.is_empty() {
                        /*return Err(TypeError {
                            message: format!("Higher kinded type encountered: {}", self.substitute(&self.substitution, &t)),
                        });*/
                        panic!("Higher kinded type encountered: {}", self.substitute(&self.substitution, &Type::Constructor(name, generics)));
                    }
                    instantiation_type.clone()
                } else {
                    Type::Constructor(name, generics.into_iter().map(|t| self.instantiate(instantiation.clone(), t)).collect())
                }
            }
            _ => t,
        }
    }

    fn substitute(&self, substitution: &[Type], t: &Type) -> Type {
        match t {
            Type::Variable(i) if substitution[*i] != Type::Variable(*i) => self.substitute(substitution, &substitution[*i]),
            Type::Constructor(name, generics) => Type::Constructor(
                name.clone(),
                generics.iter().map(|t| self.substitute(substitution, t)).collect(),
            ),
            _ => t.clone(),
        }
    }

    fn substitute_expression(
        &self,
        substitution: &[Type],
        expression: Expression,
    ) -> Result<Expression, TypeError> {
        match expression {
            Expression::Functions(functions, body) => {
                let new_functions = functions
                    .into_iter()
                    .map(|function| {
                        let new_type_annotation = function
                            .type_annotation
                            .map(|generic_type| GenericType {
                                generics: generic_type.generics,
                                uninstantiated_type: self.substitute(substitution, &generic_type.uninstantiated_type),
                            });
                        let new_lambda = self.substitute_expression(substitution, *function.lambda)?;
                        Ok(GenericFunction {
                            name: function.name,
                            type_annotation: new_type_annotation,
                            lambda: Box::new(new_lambda),
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let new_body = self.substitute_expression(substitution, *body)?;
                Ok(Expression::Functions(new_functions, Box::new(new_body)))
            }
            Expression::Lambda(parameters, return_type, body) => {
                let new_return_type = return_type.map(|t| self.substitute(substitution, &t));
                let new_parameters = parameters
                    .into_iter()
                    .map(|p| Parameter {
                        name: p.name,
                        type_annotation: p.type_annotation.map(|t| self.substitute(substitution, &t)),
                    })
                    .collect::<Vec<_>>();
                let new_body = self.substitute_expression(substitution, *body)?;
                Ok(Expression::Lambda(new_parameters, new_return_type, Box::new(new_body)))
            }
            Expression::Apply(function, arguments) => {
                let new_function = self.substitute_expression(substitution, *function)?;
                let new_arguments = arguments
                    .into_iter()
                    .map(|argument| self.substitute_expression(substitution, argument))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Expression::Apply(Box::new(new_function), new_arguments))
            }
            Expression::Variable(name, generics) => {
                let new_generics = generics
                    .into_iter()
                    .map(|t| self.substitute(substitution, &t))
                    .collect::<Vec<_>>();
                Ok(Expression::Variable(name, new_generics))
            }
            Expression::Let(name, type_annotation, value, body) => {
                let new_type_annotation = type_annotation.map(|t| self.substitute(substitution, &t));
                let new_value = self.substitute_expression(substitution, *value)?;
                let new_body = self.substitute_expression(substitution, *body)?;
                Ok(Expression::Let(name, new_type_annotation, Box::new(new_value), Box::new(new_body)))
            }
            Expression::Int(_) => Ok(expression),
            Expression::String(_) => Ok(expression),
            Expression::Array(item_type, items) => {
                let new_item_type = item_type.map(|t| self.substitute(substitution, &t));
                let new_items = items
                    .into_iter()
                    .map(|item| self.substitute_expression(substitution, item))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Expression::Array(new_item_type, new_items))
            }
            Expression::Semicolon(before, after) => {
                let new_before = self.substitute_expression(substitution, *before)?;
                let new_after = self.substitute_expression(substitution, *after)?;
                Ok(Expression::Semicolon(Box::new(new_before), Box::new(new_after)))
            }
        }
    }
}

/////////////////////////////////
// Tests
/////////////////////////////////

impl Inference {
    pub fn infer_expression(expression: Expression) -> Result<Expression, TypeError> {
        let mut inference = Inference::new();
        let initial_environment = vec![
            ("+", GenericType {
                generics: Vec::new(),
                uninstantiated_type: Type::Constructor("Function2".to_string(), vec![
                    Type::Constructor("Int".to_string(), Vec::new()),
                    Type::Constructor("Int".to_string(), Vec::new()),
                    Type::Constructor("Int".to_string(), Vec::new()),
                ]),
            }),
            ("-", GenericType {
                generics: Vec::new(),
                uninstantiated_type: Type::Constructor("Function2".to_string(), vec![
                    Type::Constructor("Int".to_string(), Vec::new()),
                    Type::Constructor("Int".to_string(), Vec::new()),
                    Type::Constructor("Int".to_string(), Vec::new()),
                ]),
            }),
            ("*", GenericType {
                generics: Vec::new(),
                uninstantiated_type: Type::Constructor("Function2".to_string(), vec![
                    Type::Constructor("Int".to_string(), Vec::new()),
                    Type::Constructor("Int".to_string(), Vec::new()),
                    Type::Constructor("Int".to_string(), Vec::new()),
                ]),
            }),
            ("/", GenericType {
                generics: Vec::new(),
                uninstantiated_type: Type::Constructor("Function2".to_string(), vec![
                    Type::Constructor("Int".to_string(), Vec::new()),
                    Type::Constructor("Int".to_string(), Vec::new()),
                    Type::Constructor("Int".to_string(), Vec::new()),
                ]),
            }),
            ("==", GenericType {
                generics: Vec::new(),
                uninstantiated_type: Type::Constructor("Function2".to_string(), vec![
                    Type::Constructor("Int".to_string(), Vec::new()),
                    Type::Constructor("Int".to_string(), Vec::new()),
                    Type::Constructor("Bool".to_string(), Vec::new()),
                ]),
            }),
            ("!=", GenericType {
                generics: Vec::new(),
                uninstantiated_type: Type::Constructor("Function2".to_string(), vec![
                    Type::Constructor("Int".to_string(), Vec::new()),
                    Type::Constructor("Int".to_string(), Vec::new()),
                    Type::Constructor("Bool".to_string(), Vec::new()),
                ]),
            }),
            ("<", GenericType {
                generics: Vec::new(),
                uninstantiated_type: Type::Constructor("Function2".to_string(), vec![
                    Type::Constructor("Int".to_string(), Vec::new()),
                    Type::Constructor("Int".to_string(), Vec::new()),
                    Type::Constructor("Bool".to_string(), Vec::new()),
                ]),
            }),
            (">", GenericType {
                generics: Vec::new(),
                uninstantiated_type: Type::Constructor("Function2".to_string(), vec![
                    Type::Constructor("Int".to_string(), Vec::new()),
                    Type::Constructor("Int".to_string(), Vec::new()),
                    Type::Constructor("Bool".to_string(), Vec::new()),
                ]),
            }),
            ("false", GenericType {
                generics: Vec::new(),
                uninstantiated_type: Type::Constructor("Bool".to_string(), Vec::new()),
            }),
            ("true", GenericType {
                generics: Vec::new(),
                uninstantiated_type: Type::Constructor("Bool".to_string(), Vec::new()),
            }),
            ("if", GenericType {
                generics: vec!["T".to_string()],
                uninstantiated_type: Type::Constructor("Function3".to_string(), vec![
                    Type::Constructor("Bool".to_string(), Vec::new()),
                    Type::Constructor("Function0".to_string(), vec![Type::Constructor("T".to_string(), Vec::new())]),
                    Type::Constructor("Function0".to_string(), vec![Type::Constructor("T".to_string(), Vec::new())]),
                    Type::Constructor("T".to_string(), Vec::new()),
                ]),
            }),
        ].into_iter().map(|(k, v)| (k.to_string(), v)).collect();
        let expect = inference.fresh_type_variable();
        let new_expression = inference.infer(initial_environment, expect, expression)?;
        inference.solve_constraints()?;
        inference.substitute_expression(&inference.substitution, new_expression)
    }

    pub fn print_infer(expression: Expression) -> String {
        match Inference::infer_expression(expression) {
            Ok(expression) => format!("{:?}", expression),
            Err(e) => e.message,
        }
    }
}

#[test]
fn test() {
    let e3 = Expression::Functions(
        vec![
            GenericFunction {
                name: "even".to_string(),
                type_annotation: None,
                lambda: Box::new(Expression::Lambda(
                    vec![Parameter {
                        name: "x".to_string(),
                        type_annotation: None,
                    }],
                    None,
                    Box::new(Expression::Apply(
                        Box::new(Expression::Variable("if".to_string(), Vec::new())),
                        vec![
                            Expression::Apply(
                                Box::new(Expression::Variable("==".to_string(), Vec::new())),
                                vec![
                                    Expression::Variable("x".to_string(), Vec::new()),
                                    Expression::Int(0),
                                ],
                            ),
                            Expression::Lambda(
                                Vec::new(),
                                None,
                                Box::new(Expression::Variable("true".to_string(), Vec::new())),
                            ),
                            Expression::Lambda(
                                Vec::new(),
                                None,
                                Box::new(Expression::Apply(
                                    Box::new(Expression::Variable("odd".to_string(), Vec::new())),
                                    vec![
                                        Expression::Apply(
                                            Box::new(Expression::Variable("-".to_string(), Vec::new())),
                                            vec![
                                                Expression::Variable("x".to_string(), Vec::new()),
                                                Expression::Int(1),
                                            ],
                                        ),
                                    ],
                                )),
                            ),
                        ],
                    )),
                )),
            },
            GenericFunction {
                name: "odd".to_string(),
                type_annotation: None,
                lambda: Box::new(Expression::Lambda(
                    vec![Parameter {
                        name: "x".to_string(),
                        type_annotation: None,
                    }],
                    None,
                    Box::new(Expression::Apply(
                        Box::new(Expression::Variable("if".to_string(), Vec::new())),
                        vec![
                            Expression::Apply(
                                Box::new(Expression::Variable("==".to_string(), Vec::new())),
                                vec![
                                    Expression::Variable("x".to_string(), Vec::new()),
                                    Expression::Int(0),
                                ],
                            ),
                            Expression::Lambda(
                                Vec::new(),
                                None,
                                Box::new(Expression::Variable("false".to_string(), Vec::new())),
                            ),
                            Expression::Lambda(
                                Vec::new(),
                                None,
                                Box::new(Expression::Apply(
                                    Box::new(Expression::Variable("even".to_string(), Vec::new())),
                                    vec![
                                        Expression::Apply(
                                            Box::new(Expression::Variable("-".to_string(), Vec::new())),
                                            vec![
                                                Expression::Variable("x".to_string(), Vec::new()),
                                                Expression::Int(1),
                                            ],
                                        ),
                                    ],
                                )),
                            ),
                        ],
                    )),
                )),
            },
        ],
        Box::new(Expression::Apply(
            Box::new(Expression::Variable("even".to_string(), Vec::new())),
            vec![Expression::Int(42)],
        )),
    );

    println!("{}", Inference::print_infer(e3));
}
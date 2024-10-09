
#[derive(Clone, Debug)]
pub struct Pgrm {
    defs: Vec<(bool, String, Term)>,
}

#[derive(Clone, Debug)]
pub enum Term {
    Lit(i32),
    Var(String),
    Lam(String, Box<Term>),
    App(Box<Term>, Box<Term>),
    Rcd(Vec<(String, Box<Term>)>),
    Sel(Box<Term>, String),
    Let(bool, String, Box<Term>, Box<Term>),
}

#[derive(Clone, Debug)]
pub enum Type {
    Top,
    Bot,
    Union(Box<Type>, Box<Type>),
    Inter(Box<Type>, Box<Type>),
    Function(Box<Type>, Box<Type>),
    Record(Vec<(String, Type)>),
    Recursive(TypeVariable, Box<Type>),
    Primitive(String),
    Variable(TypeVariable),
}

#[derive(Clone, Debug)]
pub struct TypeVariable {
    pub name_hint: String,
    pub hash: usize,
}

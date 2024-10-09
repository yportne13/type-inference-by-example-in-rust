use std::collections::HashMap;



#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Ty {
    Var(String),
    Int,
    Bool,
    Fun(Box<Ty>, Box<Ty>),
}

// 类型方案基本上是一个类型，以及一组绑定的类型变量。
// 如果你有一个类型方案 fn<T>(T) -> T，那么变量 T 在这个方案中被认为是绑定的。
// 例: a0->a0 的 scheme 是 All a0. a0 -> a0, 前面的 a0 是指类型的自由类型变量
struct Scheme {
    bound_type_vars: Vec<String>,
    typ: Ty,
}

// 在执行类型推导的时候需要找到一个 substitution 来使得两个 term 是统一的
// 比如 s S = t S, 其中 S 是substitution, t 和 s 是两个不同的 term
// 例: 对于 f x (g y) 和 f (g z) w, 要使两者相等，则 S = [x <- g z, w <- g y]
type Struct = HashMap<String, Ty>;



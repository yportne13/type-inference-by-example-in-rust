# 示例驱动的类型推断，第5部分

![图示](image.png)  
*第一个演示——lambda演算的类型推断。*

继续我们在[第4部分](../part4/article.md)中留下的内容，让我们完成类型推断的第一个版本的实现——并看一个小演示。

首先，我们需要一个语言来进行类型推断。让我们从lambda演算开始：

```rust
#[derive(Debug, Clone)]
pub enum Expression {
    Lambda(String, Box<Expression>),
    Apply(Box<Expression>, Box<Expression>),
    Variable(String),
}
```

我们还需要一个约束的表示。目前，我们只有相等约束：

```rust
#[derive(Debug, Clone)]
pub enum Constraint {
    Equality(Type, Type),
}
```

在我们遍历语法树并生成约束时，我们需要一个地方来存储它们：

```scala
val typeConstraints = ArrayBuffer[Constraint]()
```

为了生成新的类型变量，回想一下我们选择的表示方式，其中每个类型变量最初在替换中绑定到自身：

```scala
def freshTypeVariable() : TVariable = {
    val result = TVariable(substitution.length)
    substitution += result
    result
}
```

## 推断表达式的类型

现在我们需要遍历表达式并生成要解决的约束，并找出表达式的类型：

```scala
def inferType(
    expression : Expression,
    environment : Map[String, Type]
) : Type = expression match {
```

这个函数接受一个表达式和一个环境并返回一个类型。环境用于在推断过程中跟踪变量的类型。

对于lambda函数，我们为变量生成一个新的类型变量`t1`并将其添加到环境中。然后我们在这个环境中推断lambda体的类型`t2`。最终类型是函数类型`t1 => t2`：

```scala
case ELambda(x, e) =>
    val t1 = freshTypeVariable()
    val environment2 = environment.updated(x, t1)
    val t2 = inferType(e, environment2)
    TConstructor("Function1", List(t1, t2))
```

当我们遇到一个变量时，我们在环境中查找它并返回该类型：

```scala
case EVariable(x) =>
    environment(x)
```

对于应用（即，用一个参数调用函数），我们首先推断函数和参数的类型，并为返回类型生成一个新的类型变量。然后我们将函数类型约束为从参数类型到返回类型的函数类型：

```scala
case EApply(e1, e2) =>
    val t1 = inferType(e1, environment)
    val t2 = inferType(e2, environment)
    val t3 = freshTypeVariable()
    typeConstraints += 
        CEquality(t1, TConstructor("Function1", List(t2, t3)))
    t3
```

这就是`inferType`函数的全部内容：

```scala
}
```

## 完成收尾工作

我们从`inferType`返回的类型可能在这个阶段是一个类型变量。为了找出具体的类型，我们需要解决约束，然后将替换应用于类型。

由于我们目前只有相等约束，我们可以简单地使用我们在第4部分中开发的合一算法来解决它们：

```scala
def solveConstraints() : Unit = {
    for(CEquality(t1, t2) <- typeConstraints) unify(t1, t2)
    typeConstraints.clear()
}
```

应用替换是通过跟随替换链并递归地替换泛型来完成的：

```scala
def substitute(t : Type) : Type = t match {
    case TVariable(i) if substitution(i) != TVariable(i) => 
        substitute(substitution(i))
    case TConstructor(name, generics) => 
        TConstructor(name, generics.map(t => substitute(t)))
    case _ => t
}
```

我们就完成了。

## 演示

现在我们有了类型推断的第一个版本，是时候看看它的实际效果了。[在这里查看并运行迄今为止的代码](Inference.scala)。

敬请期待[第6部分](../part6/article.md)，我们将扩展类型推断到更大的语言，并将缺失的类型插入语法树。
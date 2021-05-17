---
title: A short note on typeclasses in TypeScript
author: Adam Fiedler
teaser: "At first glance, types in TypeScript do not have to make much sense to a Haskeller.
This is my attempt to resolve some of the confusion that I do not feel is handled well even in the TypeScript official handbook.
Even though one might argue that it is rather a matter of perspective
"
tags: types, typescript
---

# Problem
Let's say we want to have a polymorphic function
```typescript
function Foo<T>(val: T) {
  ...
}
```
such that a function `f :: T -> T` is defined for `val`


# Interfaces are (almost) typeclasses
Push and pop with Stack and Queue! interface Stack is not really a good example
because TypeScript ducktypes and there is not a single source of truth for the type Stack,
therefore something unexpected can happen!
Stack push/po
Let's say we define a type `Node` in TypeScript using `interface`:

```typescript
interface Num<T> {
  negate: (n: T) => T
}

type Instance<TypeClass, Type> =
```

Consider now the following two examples
```typescript
const n1 : Node<string> = {
  val: "Hello World!",
  show: () => val
}

const n2 : Node<string> = {
  val: "Yeey",
  show: () => { performSomeSideEffect(); }
}
```
In TypeScript one would say that both `n1` and `n2` are of type `Node<string>`.
However, this is slightly misleading.
We do not know neither how `f` is implemented, nor what it does.
In other words, we do not have a single source of truth for what a value of `Node<string>` is.

It might be helpful to consider that types in TypeScript are, in fact, what *typeclasses* are in Haskell.
When a value `x` is of type `Node<string>`, we only have a guarantee that we can call functions given by `Node<string>` (constants can be taken as 0-nary functions) on `x`.
However, there is no guarantee if these functions behave *consistently* when we have a collection of such values.

# Can we avoid this problem?
When we want a function on `Node` to behave consistently across any value of this type(class), we should extract it to a *typeclass*.
Really we just want to make sure that we can call a particular function in polymorphic functions, there is no need to pack this function to values.
We can simply create a typecheck and then call a regular function on the value instead.

```typescript
interface Num {
  negate: Num => Num
}

type Instance<C, T> = T extends keyof C ? C[T] : never

function Type() : Node<T> {


}

function ValueConstructor(val: T) : Node<T> {
  return {
    val: val,

  };
}

function negate(val: Instance<Num, string>) {
  return -1 * parseInt(val);
}

function f<T>(val: Instance<Num, T>) {
  //
  negate(val);
}
```

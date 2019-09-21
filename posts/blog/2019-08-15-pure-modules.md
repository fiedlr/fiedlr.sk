---
title: Is JavaScript ready for pure modules?
author: Adam Fiedler
modified: true
---

Functional programming (FP) is not new to JavaScript.
One proof of that might be the fact that there exist several helpful FP libraries like [Ramda](http://ramdajs.com).
However, the real question is if JS *itself* evolved enough to write *isolated* pieces of code in this paradigm.

With the introduction of [arrow functions](https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Functions/Arrow_functions) I came to realize that vanilla JS is much closer to the ideal of containing the necessary toolset to write what I call *pure modules*.
Let me first precisely define what I imagine a pure module is, and describe why they are a useful concept. Then I'll show why I think vanilla JS is ready for pure modules.

# Pure modules
By a pure module I mean a vanilla [ES6 module](https://exploringjs.com/es6/ch_modules.html) such that 

1) it calls only *pure* functions,
2) it defines only *constants* and *functions*,
3) it explicitly imports only other *pure* modules,
4) it contains curried functions by default,

and that all contained functions it defines

5) are *pure* functions,
6) are *arrow* functions except for value constructors,
7) return *expressions*.

Note that the Condition 2 can leave out the mention of constants because they are pretty much 0-nary functions.
Expressions in Condition 7 only mean that arrow functions are collapsed (definitions do not start with `{`). The consequence of this is that no expanded flow controls like loops are allowed.
Therefore conditions 5-7 can also be summarized as all functions that the module defines are strictly of the form (value constructors are shown at the end of the article)

```{.haskell .numberLines .line-anchors startFrom="1"}
const f = x0 => x1 => ... => xn => y
```

Pure modules are useful precisely for the reasons FP is useful.
They only force you really well to write consistent and reliable FP code in JS.
In other words, with pure modules FP becomes a matter of style which can mostly be *linted* (although there might be some problems with Condition 2 in this area).
Most importantly, marking a module as pure can give its user much advantage by providing certain assumptions. 

When a module is pure, anybody who uses it knows for a 
fact that it can easily be parallelized, unit tested and that it causes no side effects, hence it is very likely reliable. 
Code free of side effects isolated in pure modules can also lead to much better debugging tools.
Leaving out the unnecessary `;` should be safe, using a transpiler or not.

For all of these reasons I think it should be our long-term *goal* to write as many pure modules as possible, and to introduce the practice of explicitly marking correct pure modules as pure (preferably in a visible place such as their names).

# How to write pure modules

Writing pure modules is actually not hard at all.
Functions in JS are *first-class citizens* (they can be passed around as they are objects), this necessary condition is clear.
However, inspired by FP languages like Haskell, we actually need several more tools for *usable* pure modules.
In the following sections I discuss existing possibilities one by one.

## Currying & Partial application

Luckily for us, arrow functions are right folded so the aforementioned example is interpreted

```{.haskell .numberLines .line-anchors startFrom="1"} 
const f = x0 => (x1 => (... => (xn => y)))
```

Therefore currying comes natural with arrow functions without any helper functions.
Partial application works as a charm, in 

```{.haskell .numberLines .line-anchors startFrom="1"}
const modCounter = modulo => x => x % modulo
const mod5Counter = modCounter(5)
```

`mod5Counter` is really a counter modulo 5.

The only drawback is that the *order* of partially applied parameters cannot be changed in binary funcions this way (analogous to *sections*).
That is why we can also define the helpful function `flip` as follows

```{.haskell .numberLines .line-anchors startFrom="1"}
const flip = f => x => y => f(y)(x)
```

Now it is easy to define

```{.haskell .numberLines .line-anchors startFrom="1"}
const remOf5 = flip(modCounter)(5)
```

where `remOf5(x)` really calculates the remainder after dividing `5` by `x`.

What if we want to use uncurried functions as curried or vice versa?
There are numerous solutions out there already.
All in all, a `curry` function is not that difficult to define ourselves in modern JS, actually it is not that difficult to make it work for functions with any fixed number of parameters.
This is a feature that would be really handy in JS, as it is common practice to have uncurried functions with more than two arguments.
Consider the following example

```{.haskell .numberLines .line-anchors startFrom="1"}
const curryHelper = (...args) => 
  f => f(...args) 
     ? f(...args)
     : a => curryHelper(...args, a)(f)
const currify = curryHelper()
```

This `currify` works on any function with a fixed number of parameters. 
`currify` for polyvariadic functions can be done only using an ending mark because it cannot know in advance if it receives another argument. 

An `uncurrify` function works similarly and can be defined as follows

```{.haskell .numberLines .line-anchors startFrom="1"}
const uncurrify = f => 
  (...args) => args.reduce( 
    (g, x) => g(x), f
  )
```

It again works for any function with a fixed number of arguments. Curried arrow functions in JS cannot be polyvariadic by definition.

## `let ... in ...` expressions

For pure modules to be usable, we need a way how to save interim results within expressions themselves.
This is obviously important not only for saving ourselves the trouble to copy and paste the same subexpressions over and over again.
We have to take into account that the compiler has no way of knowing that it can *cache* the results.

In Haskell, one uses `let ... in ...` expressions or `where` control blocks to achieve this.  
We're again lucky in this arena.
Although arguably not as convenient to use, Immediately Invoked Function Expressions (IIFE) are able to emulate `let`s in the pure modules world as long as we do not need anything else that we could expect of `let`s. 

A quick detour just to demonstrate this fact (you can skip safely skip this). 
In Haskell, `let` ... `in` ... are expressions and not control structures such as `where` because they can be used exactly as any other expressions.
Therefore, if we have an expression of the form 
```haskell 
5 + (let x = f(5) in 2 * x * x)
```
it translates to nothing other than `5 + 2 * f(5) * f(5)`.
Notice also that the same result would be obtained moving `let x = f(5) in` higher in the expression, hence here it would not matter if we used `where` instead. 
The strength of `let ... in ...` is also evident. Unlike `where`, it can depend on *other* interim results made somewhere higher in the expression (`where` can only depend on the function input).

Back to JavaScript. When you think about it, IIFE do the same exact thing. In the following example

```haskell
const g = 5 + (x => 2 * x * x)(f(5))
```

`g` will calculate `f(5)` only once, and only then it will substitute it for `x`.
What is more, the scope of the arrow function can be extended exactly as the scope of the `let` expressions, and interim results made in a higher IIFE can be put into some IIFE lower in the chain.
The syntax can easily be changed with a transpiler.

## Composition & Chaining

As you have surely heard, function composition/chaining is very important in the FP world.
Unfortunately, there is no native operator in JS that would realize this, and we would like to avoid expressions like `f(g(h(i(j(k(l(x))))))` in pure modules.

Again either we can use a pre-existing solution or come up with our own operator `o` which would emulate function composition. 
In Haskell, for example, one can chain several functions together by using several compositions `.`.
To avoid redundant brackets resulting from JS syntax, we should make an exception and make `o` uncurried and *polyvariadic* (even though it doesn't have to be).

```{.haskell .numberLines .line-anchors startFrom="1"}
const o = (...fs) => b => 
  fs.reduceRight(
    (x, f) => f(x), b
  )
```

Now if you type something like

```{.haskell .numberLines .line-anchors startFrom="1"}
const plusplus5 = o(plus5, plus5)
```

then `plusplus5(10)` will yield 20.

## Typings

FP code without types is like a skilled hand without a body.
It is true that JS was simply not *meant* to be explicitly typed.
There have been several attempts to improve the situation with extensions like [TypeScript](https://www.typescriptlang.org/), but again I'm reviewing vanilla JS.

### Value constructors
The first idea comes again from an inspiration in Haskell. 
We can construct values using functions called *value constructors*.
Again to our luck, constructors already exist in JS and are set and called by the `new` operator. 
We can nicely force JS this way to remember the used value constructor 

```{.haskell .numberLines .line-anchors startFrom="1"}
function Integer(x) {
  this.Num = x;
}

const Ex = new Integer(x)
```

Here `Integer` is called a value constructor for the type `Num`. Values of type `Num` could be constructed using other value constructors if they were defined, such as `Natural`, `Float`, etc.
Both value constructors and type names have to be capitalized so that they are distinguishable from regular functions.

Why not pack the value and its type just in some plain object? 
Because this way the value constructor name is *reserved*.
You cannot define a different value constructor with the same name.

Using [object destructuring](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Destructuring_assignment#Object_destructuring) we can now emulate type checking in function methods like this

```{.haskell .numberLines .line-anchors startFrom="1"}
const typedPlus = ({Num: a}) => ({Num: b}) => a + b
```

## Pattern matching

There is one other advantage of using the `new` technique for value constructing.
If we use the following function to return types

```{.haskell .numberLines .line-anchors startFrom="1"}
const typeOf = v => v.__proto__.constructor.name
```

a mock object should not be able to fool pattern matching based on `typeOf` because it should return only on correctly constructed values as defined.

The current JS standard pattern matching can be made readably only over one paramater and that is either using switch or an object traversing like [shown here](https://ultimatecourses.com/blog/deprecating-the-switch-statement-for-object-literals).
Since object traversing works in collapsed arrow functions, I strictly prefer it to `switch`.

First we can create a helper function `caseOf` which does some IIFE magic so that type checking and pattern matching works at the same time.

```{.haskell .numberLines .line-anchors startFrom="1"}
const caseOf = f => v => f(v)[typeOf(v)]
```

It takes a pattern matching function over the type of `v` and returns a function taking argument which is being checked against.

You can try it out on the following example.

```{.haskell .numberLines .line-anchors startFrom="1"}
const decrement = caseOf(({Num: x}) => 
  ({ 
    Integer: x - 1, 
    Natural: x > 0 ? x - 1 : 0
  }) 
)
```

Trying a different type like `decrement("x")` fails, exactly as trying a non-matching value constructor such as `decrement(new Float(2.5))`. 

This is only a proof of concept. 
For a more usable pattern matching such as matching over more than one parameter, new JS syntax would probably need to be implemented.

It would be a much needed addition to the language, after which better support for pure modules could come closer to reality.

# Conclusion

I've defined what I imagine behind a pure modules and argued for their introduction to existing codebases. 
The idea is to introduce a practice of creating isolated JS places containing strictly guidelined FP code. 
Most importantly, the idea is also to label these places as pure modules visibly so that developers can make assumpions about the functions they export.

Then I've hopefully showed some hints that vanilla JS should be ready for complex pure modules to a certain extent.
To make sure of that, the logical next step is to try creating a whole project based upon pure modules with a very limited side-effect scope.
If you create such a project in the mean time before I make time to do so, make sure to let me know.

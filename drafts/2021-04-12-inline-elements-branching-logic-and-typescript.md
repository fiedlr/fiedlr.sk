---
title: Inline elements, branching logic and TypeScript
author: Adam Fiedler
teaser: "In this article we look at how certain types of properties (pun intended)
can be checked with TypeScript.
More "
tags: types, typescript, branching-logic, ctl
---

Everyone who works with TypeScript has seen on numerous occasions the common use
of types in TypeScript like

```typescript
interface User<T extends "subscriber" | "basic"> {
  readonly type?: `${T}`;
  name: string;
  age: number | "Nothing";
  contact: number;
  creditCard: T extends "subscriber" ? string : "Nothing";
  children: User<T>[];
}
```

Even with the more advanced features like literal types, recursion and conditional types,
purposely avoding `undefined`, a technique with the optional attribute to make type inference easier
using a disjoint attribute, there is nothing out of the ordinary.
We would like to look at a more complex type of property.

This property can be illustrated with a very familiar example.
In HTML, there are elements called *inline* elements that can only contain other
inline elements in a valid HTML file.
Say, the element `span` cannot contain the block element `div`.
However, it is obviously okay the other way around.

What if we were to represent the DOM tree as a type `Node` such that we
could check this always holds?
In other words, TypeScript would throw an error in case we had a "tree" that
did not match our intended structure?
I guess there are many ways how to do this, however, not every way leads to a
usable solution.
It requires some thinking to proceed.

# Representing the DOM
First working thing that comes to mind is simple type hierarchy.
```typescript
interface Node {
  readonly tag: string;
  block?: "true" | "false";
  id?: string;
  className?: string;
  children: Node[];
}

interface InlineNode extends Node {
  block?: "false";
  children: InlineNode[];
}
```

Albeit working, we would like a solution that
1) avoids attribute/type overriding (for the obvious reasons),
2) is more flexible (as will be seen later).

We will need a generic type `Node<T>`.
This is because we need to enforce the children of our nodes to behave according to their parent, which can only be passed down with another type `T`.

```typescript
interface Node<T> {
  readonly tag: string;
  id?: string;
  className?: string;
  children: T[];
}
```

However, the challenge is how to define the child types, block/inline elements such that we do not need any generics.
The solution
```typescript
interface Inline<T extends Inline<T>> extends Node<T> {}
```
is messy because now we always have to do something like

```typescript
const span: Inline<T> = {
  ...
}
```

and we cannot get `T` in any reasonable or automatic way!
The idea is to have the type `T` being gradually built by TypeScript itself somewhere behind the scenes.
TypeScript will then check if our children (and their children) element match the required type `T`.
This concept will hopefully become clear once we get to define exactly what I have in mind.

# Inline elements
First we define two types, block elements and inline elements.
Block elements can contain `any` elements, therefore the choice is clear.
However, inline elements can only contain other inline elements (i.e., elements of the same type) as we said.

```typescript
interface BlockNode
extends   Node<any> {
  block?: "true";
}

interface InlineNode<T extends InlineNode<T>>
extends   Node<T> {
  block?: "false";
}
```

Put into TypeScript, we want the inline elements to
1. be a `Node<T>`, that is why we extend `Node<T>`,
2. the `T` type needs to extend an inline element (TypeScript knows, of course, that the two `T`'s are the same).

With the optional literal type, we just make sure that they are disjoint so that TypeScript can differentiate block/inline elements on the type level.

You might have noticed we still haven't solved the explicit `T` type problem.
One of the ways how to let `T` be built by TypeScript itself is to *declare* beforehand what elements are inline like this.

```typescript
type InlineTags = span | input;

interface span extends InlineNode<InlineTags> {
  tag: "span";
}

interface input extends InlineNode<InlineTags> {
  tag: "input";
}
```

This seems kind of crazy at the first glance.
When you look at it from the perspective that `InlineTags` just gets rewritten to `span | input`, it is a conventional type recursion.
But now it is easy to define a block element like

```typescript
interface div extends BlockNode {
  tag: "div";
}
```

Now we can just check this out!
```typescript
const sp1: span = {
  tag: "span",
  children: []
}

const inl: input = {
  tag: "input",
  children: []
}

const sp2: span = {
  tag: "span",
  children: [sp1, inl]
}

const divElem: div = {
  tag: "div",
  children: [sp1, sp2]
}

/* Throws an error */
const sperror: span = {
  tag: "span",
  children: [divElem]
}
```
Notice how TypeScript happily substitutes for `T` in the error mesage.
The role of `T` is twofold.
1. It allows to impose a property/specification on the children.
2. It serves TypeScript to actually verify if this property holds in the objects we declare to be of this type.

This leads us to the question whether we could not further extend it to a more general technique.

# Branching Logic
The next paragraphs are meant for those who are looking to put this into a little broader context.
What I will show you is not easy and not directly useful for frontend development per se.
However, it might help you think more clearly about verifying properties and what we are actually achieving with the type system.
In the end, this is what TypeScript is for - to ensure that the objects that we deal with satisfy certain properties so that we know what to expect.

Here we want to deal with a certain kind of properties, let us called them *branching properties*.
These could be type hierarchies but let us stick to the DOM tree example.
Imagine the DOM tree as a set of paths starting in the root `<html>`.

          html
      /            \
    head          body
      |       /     |     \
     ...    div    span   ...
             |      |
            div     a
             |
            ...

Other than what we have seen, there are many things you may want to consider here.
For example you could enforce that every element has an id or a className, or
that no div has more than two div parents, or that if we use `<figcaption>`, its direct parent can only be `figure`, or that there exists a path to a footer with helpful links, etc.

These are not just some arbitrary properties that we define for the DOM tree.
In fact, on an abstract level they have been studied for a long time in an area of Computer Science called formal verification.
Instead of speaking directly about properties in English, we use formalisms to capture their essence.
Then we can model real world systems in these formalisms and specify which properties we expect to hold in them.

One of the formalisms that are often used is called Computation Tree Logic (CTL) on finite paths (because our DOM tree is finite).
The formal syntax of CTL consists of *atomic formulas* like `p` or `q`, *logical operators* like `/\` (conjunction of formulas) or `=>` that are analogous to propositional logic and *temporal operators* like `AG`, `AX`, etc., which will informally be explained shortly.
Then you write down specifications like `node |= phi` that says starting in `node`, the formula `phi` holds.

# Branching Logic and Inline Elements
Coming back to our earlier example with inline elements, this property can be simplified in English to `every inline element contains only inline elements`.
More precisely, `for every inline element A, if an element B is a child of A, then B is an inline element`.
This is easy to specify in CTL because the mentioned *atomic formulas* can represent types we have worked with.

On a very abstract level, we can assume that a `Node<T>` either is an `InlineNode<T>` or it is not.
We drop the `T` because it is only for TypeScript purposes and write `InlineNode` as a *property* that either holds in some element or it does not.
In other words, we declare `InlineNode` as an atomic formula mentioned earlier.
Then we can write `node |= InlineNode` meaning that `node` is an `InlineNode` (`InlineNode` *holds* in `node`).
For example if we write `node |= AX InlineNode`, then we mean that all direct children of `node` are inline elements, etc.

Considering all this, the specification we are aiming for can be `html |= AG (InlineNode => AX InlineNode)`.
This means that on `A`ll paths starting in the node `html`, `G`lobally holds that if an element A is of type `InlineNode`, then `A`ll ne`X`t elements (i.e., direct children of A) are also of type `InlineNode`.
We can draw an example of a correct tree to explicate this.

          html
      /            \
    head          body
      |       /     |                     \
     ...    div    span: InlineNode         ...
             |      |               \
            div    span: InlineNode   a: InlineNode
       /     |      |
     div    ...     a: InlineNode
     /
    span: InlineNode
       |              \
    a: InlineNode   a: InlineNode

As you can see, once you reach an "island" of InlineNodes, you cannot ever escape.
It does not matter what path in the tree you choose.
The operator `AG` means that it holds in `A`ll paths of the (sub)tree and in every element of the path (`G`).
If we wrote `AF` instead of `AG`, for example, it would mean that on all paths we eventually (`F`inally) reach some node satisfying that its children
This is very different, especially because

# Checking other CTL properties with TypeScript
We finally return to why I haven't used the natural.
The framework framework can be easily generalized to something of the following.
```typescript
interface Node<Prop> {
  readonly tag: string;
  id?: string;
  className?: string;
  children: List<Prop>;
}
```
where `List<Prop>` is a *dependent type*!


# Conclusion
There are many tools that allow you to check if certain properties hold in a given system.
It is interesting that type systems and TypeScript can be used for this as well in a way.

# Appendix
One may wonder why I did not choose the following, easier, approach.
```typescript
interface Node {
  readonly tag?: string;
}

interface BlockElement {
  block?: "true";
  children: Node[];
}

interface InlineElement {
  block?: "false";
  children: InlineElement[];
}
```
While a valid objection, the answer is that it simply is not flexible enough.
Consult the previous section.

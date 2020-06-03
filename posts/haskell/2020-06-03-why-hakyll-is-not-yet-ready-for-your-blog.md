---
title: Why Hakyll Is Not Yet Ready for Your Blog
author: Adam Fiedler
teaser: "I never believed I would be writing this. Don't get me wrong. I love both Haskell and Hakyll, which builds upon using one of the strongest conversion tools the world has known: Pandoc.
Needless to say that this very blog runs on Hakyll.
However, nothing can advance without somebody laying out loud the things that don't work.
And with Hakyll it seems like such a list is long overdue."
---

TLDR: I've spent countless of hours to make *basic* things work and I *still* do.
Ok, maybe if I were a professional Haskeller and knew how to set up [stack](https://docs.haskellstack.org/en/stable/README/) for better debugging than just mere "try&error", it would've taken me half this time.
The number of hours would still be in dozens though.
Unless you are a complete masochist and just adore Haskell, wishing to be a contributor yourself, I do not yet recommend using Hakyll for your new blog.
There's an awesome [team spirit and progress](https://github.com/jaspervdj/hakyll/pulls?q=is%3Apr+is%3Aclosed), nonetheless, it simply isn't there yet.
For your own mental health, use a more matured static site generator like [Jekyll](https://jekyllrb.com) or even a super advanced [Gatsby](https://www.gatsbyjs.org), if you have experience with React.

# Problems

At the beginning of my story with Hakyll, I just knew that I was eager to use Haskell for something more than just theoretical exercises.
I also wanted to start a blog when I ran upon Hakyll.
Since I'm a huge fan of [*literate programming*](https://www-cs-faculty.stanford.edu/~knuth/lp.html) and freedom of expression, being able to use [almost any input format](https://pandoc.org/) sounded very tempting.
Hakyll thus seemed a perfect fit for me.

I started with the basic template that comes with the fresh installation and sort of tinkered with it.
Although it did not provide much functionality, I've counted with this.
After all, it's a generator and not a standalone CMS.
I thought the [tutorials](https://jaspervdj.be/hakyll/tutorials.html) would answer most of my needs.
But the truth is worse, let's face it.

It turns out Hakyll is still just a fancy *wrapper* for a Pandoc fragment (converting to HTML).
It's not even a [Gentoo](https://www.gentoo.org) in the static generator world, it is rather an [LFS](http://www.linuxfromscratch.org).
Almost nothing basic works out of the box: there's no pagination, no tags, no categories, no feeds, no sitemaps, no proper *excerpt support* for free (I gave up and just use a basic YAML tag for them, even though you can have fun [here](https://jaspervdj.be/hakyll/tutorials/using-teasers-in-hakyll.html)).
You start with the newest 5 posts, which you can add to the `posts` folder and that's it.
And yes, this dumb prefix `posts/` is in the link to any post (that is actually the first thing I was solving).

Everytime you want another feature, not only you have to read through the tutorials, you have to dig up blog posts or reports by Hakyllers from pre-historical era (not that you would have enough of software archeology at work).
You're superlucky when you find a tutorial that provides a solution for what you set out to achieve.

The mission is not accomplished there: you must also pray that the given fellow Hakyller's guidance still works.
Many times it does not and you burn more hours finding a way around it in the API.
Haddock is nice in theory and all, but it is often not a thorough documentation describing how the smaller parts work together (at least not in the Hakyll's case).

To make it more fun, solving things takes various forms.
Sometimes you just need to add some super tweaky special tag to your YAML blocks (that you dig up in the Pandoc docs).
Sometimes you have to create new compilers.
Sometimes you have to edit existing compilers.
Many times you have to copy and paste from Hakyll's codebase to tweak something slightly because it is so tightly coupled you cannot do it from without.
The problems are neverending.
Let me illustrate on the examples I've encountered.

## Syntax highlighting
Ouch.
Almost all of the pandoc templates are awful.
I've tried to google some better templates but to no avail.
I've ended up spending half a day to mimic syntax highlighting from VSCode (and if you're wondering, nope, JSX support is nonexistent).
Maybe I'm just a perfectionist, but seriously, there could be a repo or something where we could collect these syntax templates.

It doesn't end there.
The pandoc syntax highlighter has a very limited possibilities.
It actually does support line numbering (after some googling you find out about `.numberLines`),
but then you have to look up how to reference those lines.
Nice, that actually exists too...
But then you open up your slick blog and find out the styling is screwed up again.
And don't even try using wordwrapping because you'll screw it up yet again!

## TeX
TeX support is great, but not that great.
Although this is not Hakyll's fault per se (more of a Pandoc one), it is exhausting to fix half the things you would expect from TeX.
Would you like something more complicated like [tikz](http://www.texample.net/tikz/examples/)?
Forget it.
Maybe there's a way, I still haven't looked at it in detail, to be honest.
But I bet it won't be easy.

Ok let's say some basic equations aligning.
Referencing anything within your document is awful.
For internal links in LaTeX sources use the standard `\label{id}` with `\protect\hyperlink{id}{link name}`.
No, `\ref` does not work... This took me another long hours.

Remember that Hakyll is a wrapper for a Pandoc *fragment*?
It exports to HTML, that's it.
You have to struggle with writing custom compilers pretty much for anything else, that includes *PDF*, of course.
Maybe a repo with common case compilers would be nice, when they're for some reason not included in the basic codebase.
PDF is not really that superadvanced (come on, we're talking Pandoc here).

PDF export

## Embedding Haskell in templates
Would you like some ad-hoc logic in the templates?
You're out of luck.
The templates support trivial `if` blocks that check for a truth value of a [field](https://jaspervdj.be/hakyll/reference/Hakyll-Web-Template.html), optionally with an `else` block, plus `for` loops for ranging over list-ish fields (otherwise it would be really painful to template a list).
Great, but think of what happens when you want a new field.
You have to *recompile* the site executable every time.
Can you imagine how long that takes?

I understand that it tries to keep things simple, but is it surely the only way to make things work?
All the more when we have such fruitful things as [GHCJS](https://github.com/ghcjs/ghcjs).
Why cannot I put some logic in the templates and make use of it in the macros?
I understand Haskell doesn't have a direct access to DOM and I can use JS, but I don't have access to everything in JS (such as other loaded templates).
Even an awful language like PHP can do something like
```php
Hello <?php echo "World! 1 + 1 ="; echo 1 + 1; ?>
```
(yes, the result is [indeed 2](https://en.wikipedia.org/wiki/Principia_Mathematica)!)

## Categories
There actually is a module for categories, but it is a very .
It is mixed up with tags, which would make sense if it wasn't in the same file and all mingled together.
What I

## Sitemaps

## Lack of useful guides and documentation
Everything

# Possible solution
It is possible that many of my objections are caused due to my lack of thorough understanding of the Hakyll ecosystem.
However, I believe that does not void many of my arguments.
An opensourced code is useful only insofar as it is useful for *beginners*.
Nobody should spend hours digging through just to create a simple blog.
I'll be very happy for feedback.

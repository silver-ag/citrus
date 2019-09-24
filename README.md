
# Citrus

Citrus is a lisp<sup>1</sup> with more syntax. It's also a programming language programming language.
It implements the following syntax on top of the usual s-expressions and 'quote `quasiquote ,unquote abbreviations:

- `|` final-lists
I find it a little awkward that lisps often require too many closing parens to intuitively count all at once. In citrus, you never have to type more one `)` next to another. Hopefully, it'll turn out that this format makes the code easier to read (though I'm not so optimistic about ease of writing).

Multiple close-parens happen only when the last element of a list is itself a list. In citrus, you can (optionally) open a list which is the last element of the list it's in with `|` instead of `(`.

If you do, then you don't have to explicitly close it - it closes automatically when the list it's part of does.

```
(write |cons 1 '|2 3) -> (write (cons 1 '(2 3)))
```

- `[n]` list accessors
tired of typing `(list-ref (list-ref (list-ref l 1) 3) (+ n 2))`<sup>2</sup>? citrus allows the traditional array accessor `[n]` to work on lists: `l[1][3][(+ n 2)]`

```
(list 1 2 3)[1] -> (ref (list 1 2 3) 1)

```
for the avoidance of doubt, `'(a b c)[1]` normalises to `(ref (quote (a b c) 1))`, not `(quote (ref a b c) 1)`, and `'(a (b c d) e)[1][2]` normalises to `(ref 2 (ref 1 (quote (a (b c d) e))))`

- `:{lang ... lang}` sublanguage blocks<sup>3</sup>
remember how citrus is a programming language programming language? You can embed different languages' source in citrus code<sup>4</sup>. langauges are a type, so you can (will be able to) do things like
```
(make-python 2.7):{lang print 'hello world' lang}
```
given a `(make-python n)` function that produces a language object equivalant to python version `n`.
```
my-language:{lang source code; lang} -> (apply-lang my-language " source code; ")
```

- strings are lists
citrus has no string type, just lists of chars (io functions automatically print a list of all chars as a string)
```
"hello world" -> (#h #e #l #l #o #space #w #o #r #l #d)
```


as shown at the end of each section, all of these are transformed into more verbose forms which are also valid.

### General Docs
In a note format for now:

character literals are written like so:
 `#c`
 where `c` is:
  a single character -> that literal character
  `\n` or `\t` -> newline or tab
  `space` -> a literal space character
  `\x` followed by two hex digits -> the character corresponding to that ascii code

rather than `(list-ref list index)`, citrus has `(ref index list)`. in general citrus tries to put indices before the thing they point into, because they're likely to be smaller and simpler and it feels easier to make sense of.


### Roadmap

- add quasi- and unquote
- decide what we can import from racket
- write things we can't
  - includes write, print, display, printf, pretty much anything that gives output really
- if the racket macro system can't be imported, write one
- if the racket require form can't be imported, write one
- implement first-class language objects and (apply-lang)
- type inference system, giving warnings only (by default, I guess there's no reason not to have a strict mode where it won't run at all if not type safe)
- reimplement outside of racket? self-host?

whenever:
- optimise expander, in particular figure out which recursive (ct-module-special-form) calls aren't actually needed (like in (atom), probably)



1: specifically, it's mostly racket

2: to be fair, I can see why it's done that way. I feel that (for me, at least), citrus' alternative syntax is at least not too much harder to reason about to outweigh the ease of reading simpler cases

3: not yet implemented. well, the syntax is, but `(apply-lang)` isn't defined yet (and neither is the first-class language type for that matter)

4: provided the source you want to evaluate doesn't contain `lang}`. I want to allow user-defined delimiters, but it'll require some messing around in the lexer that I'm putting off

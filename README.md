
# Citrus

[not in a usable state yet]

Citrus is a lisp<sup>1</sup> with more syntax. It's also a programming language programming language.
It implements the following syntax on top of the usual s-expressions and 'quote `quasiquote ,unquote abbreviations:

### `|` final-lists
I find it a little awkward that lisps often require too many closing parens to intuitively count all at once. In citrus, you never have to type more one `)` next to another. Hopefully, it'll turn out that this format makes the code easier to read (though I'm not so optimistic about ease of writing).

Multiple close-parens happen only when the last element of a list is itself a list. In citrus, you can (optionally) open a list which is the last element of the list it's in with `|` instead of `(`.

If you do, then you don't have to explicitly close it - it closes automatically when the list it's part of does.

```
(write |cons 1 '|2 3) -> (write (cons 1 '(2 3)))
```

### `[n]` list accessors
tired of typing `(list-ref (list-ref (list-ref l 1) 3) (+ n 2))`?<sup>2</sup> citrus allows the traditional array accessor `[n]` to work on lists: `l[1][3][(+ n 2)]`

```
(list 1 2 3)[1] -> (ref 1 (list 1 2 3))

```
for the avoidance of doubt, `'(a b c)[1]` normalises to `(ref 1 (quote (a b c)))`, not `(quote (ref 1 (a b c))`, and `'(a (b c d) e)[1][2]` normalises to `(ref 2 (ref 1 (quote (a (b c d) e))))`

### `:{lang ... lang}` sublanguage blocks<sup>3</sup>
remember how citrus is a programming language programming language? You can embed different languages' source in citrus code<sup>4</sup>. langauges are a type, so you can (will be able to) do things like
```
(make-python 2.7):{lang print 'hello world' lang}
```
given a `(make-python n)` function that produces a language object equivalant to python version `n`.
```
my-language:{lang source code; lang} -> (apply-lang my-language " source code; ")
```

### strings are lists
citrus has no string type, just lists of chars (io functions automatically print a list of all chars as a string)
```
"hello world" -> (#h #e #l #l #o #space #w #o #r #l #d)
```


as shown at the end of each section, all of these are transformed into more verbose forms which are also valid.

## General Docs
In a note format for now:

character literals are written `#c`, where `c` is:
- a single character -> that literal character
- `\n` or `\t` -> newline or tab
- `space` -> a literal space character
- `\x` followed by two hex digits -> the character corresponding to that ascii code


rather than `(list-ref list index)`, citrus has `(ref index list)`. in general citrus tries to put indices before the thing they point into, because they're likely to be smaller and simpler and it feels easier to make sense of.

`(write)`, `(display)` and `(print)` are modified to take any number of arguments to output and an optional #:out output port argument. they also return their arguments as a list, rather than #<void>

`(parse grammar text)` takes a grammar created with `(grammar (production name element1 ... elementn)... (terminal name "regex"))` and parses text string into an AST. valid `element`s in production rules are characters, symbols that are the names of other rules, and special forms `(* element)`, `(or element1 ... elementn)`, `(? element1 ... elementn)` and quoted lists `'(element1 ... elementn)` - for instance, `(production example (or '(#a (? term1)) (* '(term2 term3))))` which matches either the character 'a' followed by term1 or any (>0) amount of repetitions of term2 followed by term3.

Racket's `require` form is available in citrus. However, some things from racket don't work when loaded for a repl (ie. racket -f or in drracket) - for instance the gui system and probably web server - because once the main thread reaches the end it looks for #%top-interaction and crashes the whole program when it's not there. I don't want to provide #%top-interaction because the repls mentioned automatically use racket's read-syntax, rendering them useless for citrus.

## Roadmap

- decide what we can import from racket
  - meanwhile, write things we can't
    - includes write, print, display, printf, pretty much anything that gives output really
  - add contracts to definitions.rkt provisions
- implement first-class language objects and (apply-lang)
- self-host using language tools
- working repl (default #%top-interaction uses racket read-syntax)
- type inference system, giving warnings only (by default, I guess there's no reason not to have a strict mode where it won't run at all if not type safe)

whenever:
- optimise expander, in particular figure out which recursive (ct-module-special-form) calls aren't actually needed (like in (atom), probably)
- make an editor that supports | in bracket matching? or at least a slightly better syntax highlighter for drracket.


1: specifically, it's mostly racket

2: to be fair, I can see why it's done that way. I feel that (for me, at least), citrus' alternative syntax is at least not too much harder to reason about to outweigh the ease of reading simpler cases

3: not yet implemented. well, the syntax is, but `(apply-lang)` isn't defined yet (and neither is the first-class language type for that matter)

4: provided the source you want to evaluate doesn't contain `lang}`. I want to allow user-defined delimiters, but it'll require some messing around in the lexer that I'm putting off

# Citrus

It's basically racket, with a few changes:
- `|` syntax
- strings are lists
- modified output functions
### `|` Syntax
This is by far the most important change from racket. Fairly often in lisp/schemes, you find yourself closing a lot of brackets at once:
```
(define (fib n (a 0) (b 1))
  (if (< n 2)
      1
      (+ a (fib (- n 1) b (+ a b)))))
```
You may notice that you only have to close more than one bracket at a time if the last element of a list is itself a list. In Citrus, `|` is like `(` except that it indicates that the list you're opening is the last element of the list it's part of, and so it will be automatically closed at the same time:
```
(define (fib n (a 0) |b 1)
  |if (< n 2)
      1
      |+ a |fib (- n 1) b |+ a b)
```
Which feels a bit cleaner to me, although it's probably inherently harder to understand. I never said this language was any good, after all.
### Strings are lists
Instead of strings being a seperate datatype, `"hello world"` is a synonym for `(#\h #\e #\l #\l #\o #\space #\w #\o #\r #\l #\d)`. This means you can use list operations on strings directly.
```
> (third |reverse "abcdefg")
#\e
```
### Modified output functions
`display`, `write` and `print` are modified in two ways: they automatically render lists of only characters as strings (same thing, remember?), and they take any number of arguments and concatenate them together. At some point I'll get round to adding a keyword argument for the output port.
```
> (define w "world")
> (display "hello " w)
hello world
```

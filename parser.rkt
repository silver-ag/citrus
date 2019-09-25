#lang brag
ct-program: (whitespace* ct-expression*)* whitespace*
ct-expression: (ct-list |ct-atom | ("'" | "`" | ",") ct-expression)  [("[" ct-expression "]")*]
ct-list: QUOTED-STRING | "(" (whitespace* ct-expression*)* whitespace* [ct-final-list] ")"
ct-final-list: ["'" | "`" | ","] "|" (whitespace* ct-expression*)* whitespace* [ct-final-list]
ct-atom: lang-block-cont | lang-block | ct-char | ct-symbol | ct-number | ct-expression
lang-block: ct-expression ":" lang-block-cont
lang-block-cont: LANG-BLOCK-CONTENT
ct-number: NUMBER
ct-char: CHAR
ct-symbol: SYMBOL
whitespace: WHITESPACE
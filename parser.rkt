#lang brag
citrus-program: (ct-number | ct-symbol | ct-list | ct-id )*
ct-list: QUOTED-STRING | "(" (ct-number | ct-symbol | ct-list | ct-id)* (ct-final-list | ct-final-id-list | ")")
ct-final-list: "|" (ct-number | ct-symbol | ct-list | ct-id)* (ct-final-list | ct-final-id-list | ")")
ct-id: "'" (ct-list | ct-number | ct-symbol | ct-id)
ct-final-id-list: "'" "|" (ct-number | ct-symbol | ct-list | ct-id)* (ct-final-list | ct-final-id-list | ")")
ct-number: NUMBER
ct-symbol: SYMBOL
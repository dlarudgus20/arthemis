# keywords

- `var`
- `fun`
- `if`
- `while`
- `do`
- `end`
- `break`
- `continue`
- `else`
- `elif`
- `false`
- `true`
- `for`
- `null`
- `then`

# operators

precedence | left-to-right | right-to-left | unary
---|---|---|---
0  | `()`
1  | `.` `[]`
2  | | | `+` `-`
3  | function call expression
4  | `*` `/` `%`
5  | `|` `&` `^`
6  | `+` `-`
7  | `<` `<=` `>` `>=` `=` `!=`
8  | `&&`
9  | `||`
10 | | | `!`
11 | | `=`

# expression

- *`identifier`*
- string literal
- numeric literal
- `null` literal
- <code>{ *[ expr1, ... ]* }</code>
- <code>( *expr* )</code>
- <code>*expr* [ *expr* ]</code>
- <code>*function-expr* ( *[ arg1, ... ]* )</code>
- <code>*expr1* *infix-op* *expr2*</code>
- <code>*prefix-op* *expr*</code>
- <code>fun *identifier* ( *[ identifier1, ... ]* ) begin *block-content* end</code>

# statement

 - *block-content* is <code>*[ statement1 (\<newline\>|;) ... ]*</code>.
 - *loop-content* is <code>*[ (* break * |* continue * | statement1) (\<newline\>|;) ... ]*</code>.

- `break`
- `continue`
- <code>var *identifier* = *expr*</code>
- <code>begin *block-content* end</code>
- <code>if *expr* then *block-content* *[* elif *expr* then *block-content ... ]* *[* else *block-content ]* end</code>
- <code>while *expr* do *block-content* end</code>
- <code>for *identifier* in *expr* do *block-content* end</code>

# intermediate representation (*.muni*)

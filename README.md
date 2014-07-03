Introduction
------------

The template system of C++ is strong enough to be able to express a class of
compile time computations. This "feature" has been exploited for generic
programming as well as for precomuptation of various lookup tables and so on.

A classic example is a compile time factorial:

    template <int n>
    struct factorial
    {
    	enum { value = n * factorial<n - 1>::value };
    };

    template <>
    struct factorial<0>
    {
    	enum { value = 1 };
    };

(code from [Wikipedia article on template metaprogramming](http://en.wikipedia.org/wiki/Template_metaprogramming))

This languages takes this idea to the extreme. `tmpcompiler` complies a small
functional language with Haskell-like syntax into a templated C++ code that
can be compiled and "run" at compile time - much like the example above.

Syntax
------

This is a (very rough) grammar for the language:

    module ::= top-level ";" module | λ

    type ::= type "->" type  -- Function type.
           | type type       -- Type application.
           | name            -- Type constructor.
           | ty-var          -- Type variable.

    top-level ::= data-def | value-def | type-sig | assumption

    data-def    ::= empty-dd | nonempty-dd
    empty-dd    ::= "data" ty-con           -- No constructors.
    nonempty-dd ::= "data" ty-con "=" cons  -- At least one constructor.

    ty-con  ::= name ty-vars  -- Name of the type constructor followed
                              -- by type variables.

    ty-vars ::= ty-var ty-vars | λ

    cons ::= con "|" cons | con
    con    ::= name fields       -- Constructor name followed by field types.
    fields ::= type fields | λ   -- Types of all fields.

    value-def ::= name vars "=" expr

    expr ::= var
           | "\" var vars "->" expr -- Lambda abstraction.
           | expr expr              -- Application.
           | "let" decls "in" expr  -- Local definitions.
           | expr ":" type          -- Explicit type.
           | natural                -- Number literal.
           | "True" | "False"       -- Boolean literal.
           | prefix expr            -- Prefix operator.
           | expr infix expr        -- Infix operator.

    prefix ::= "~"  -- Unary minus.
             | "!"  -- Unary negation.

    infix ::= "*"  | "/"  | "%"  | "+" | "-" | "<" | "<=" | ">" | ">="
            | "==" | "/=" | "&&" | "^" | "||"

    vars ::= var vars | λ

    type-sig ::= name ":" type

    assumption ::= "assume" name ":" type

Inside an expression, names starting with a lower case letter refer to
variables, while names starting with an upper case letter refer to constructors.
In a type, lower case names refer to type variables and upper case names to
type constructors.

`let`, `in`, `data`, `True`, `False` and `fix` are reserved keywords. Since we
are compiling the code into C++, all C++ keywords are also reserved in this
language with the addition that upper case names cannot be used as type
constructors if their lower case variant conflicts with a C++ keyword.

`--` are line comments and `{- -}` are multiline comments. For more information
about the grammar, check the modules `Compiler.Lexer` and `Compiler.Parser`.

Language
--------

The language is basically a lambda calculus with
[Hindley-Milner type system](http://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system),
recursion and a few extra constructs - `let` and `data`.

Higher-order functions and currying are fully supported. Recursion is only
supported in the form of implicit transformation to the fixed point operator
`fix`. Other forms of recursion (such as mutual or polymorphic recursion) are
not supported.

The language is parsed and processed in a strictly top-down manner, which means
that defined value or data type can only be used in the part of the code that
follows its definition - with the exception that the name of the defined
value (type) is in scope in the following expression (constructors). This is
also true for local definitions inside `let`.

Pattern matching is not supported since we cannot guarantee correct evaluation
in the resulting C++ code. Instead, definition of a data type automatically
defined an eliminator. As an example:

    data D a b = A a | B b (D a b) | C

    -- Automatically defined.
    d : (a -> z) -> (b -> D a b -> z) -> z -> D a b -> z

The eliminator `d` has following runtime behaviour:

    d f g h (A x)   => f x
    d f g h (B x y) => g x y
    d f g h (C)     => h

The name of these eliminators is given by the name of the type constructor -
the first letter of its name is converted to lower case.

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
defines an eliminator. As an example:

    data D a b = A a | B b (D a b) | C

    -- Automatically defined.
    d : (a -> z) -> (b -> D a b -> z) -> z -> D a b -> z

The eliminator `d` has following runtime behaviour:

    d f g h (A x)   => f x
    d f g h (B x y) => g x y
    d f g h (C)     => h

The name of these eliminators is given by the name of the type constructor -
the first letter of its name is converted to lower case.

Runtime
-------

Freshly generated code will contain references to `fix`, `__data`, `__dummy` and
also possible built-in functions (`int` operations, `bool` operations, etc). To
be able to use the resulting code, a runtime has to be included.

The inclusion is as simple as copying the `runtime` directory to wherever the
resulting code is. You can also control the include location using the compiler
flag `-i` or `--includedir`, which changes the `#include` directives in the
generated code. Standalone code can also be created using the flag `-a` or
`--addruntime`, which causes the compiler to copy the runtime into each
generated file.

The runtime has two parts: `runtime/fix.hpp` is a template that implements
the fixed point combinator `fix` and is needed whenever the original code
contains recursion. `runtime/data.hpp` contains templates and structures
needed for correct implementation of user defined data types and function
application; it also contains definitions of built-in types (`Int` and
`Bool`) and various functions that operate with values of these types.

To allow easy integration with existing metaprogramming code, the language
supports an abstract type `Type` which represents a C++ type and `assume`
construct which allows to bring in scope values that can be defined elsewhere.

For example, if we have operation `add_ptr` implemented on the C++ level:

    struct add_ptr
    {
        struct type
        {
            template <typename T>
            struct apply
            {
                typedef typename T::type* type;
            };
        };
    };

We can easily bring this operation in scope by writing:

    assume add_ptr : Type -> Type

Usage
-----

The compiler is usually used with the following command:

    ./tmpcompiler -o output input

This type checks the file `input` and if no error is found, it complies the
source into C++ code and writes it into `output`.

If `-a` is used, the user shoud also specify the location of the runtime using
`-i`:

    ./tmpcompiler -a -i path/to/runtime -o output input

The resulting code can be used either directly (if the defined value is not
a function) - the computed type is available inside an inner type `type`.

    a : Int;
    a = 4

    -- Resulting C++ code.
    struct a { ... };

    a::type        == Int<4>
    a::type::value ==     4

User defined types are encoded in a more complex way:

    __data<constructor-number, __dummy, field1, ..., fieldN>

Where `constructor-number` is given by the order in which constructors of its
type are defined. For example:

    data T = A  -- 0
           | B  -- 1
           | C  -- 2

The value itself can again be accessed using the inner type `type`:

    data List a = Nil | Cons a (List a);

    a : List Int;
    a = Cons 1 Nil

    -- Resulting C++ code.
    struct a { ... };

    a::type == __data<1, __dummy, Int<1>, __data<0, __dummy> >
    --                ^           ^       ^      ^
    --                |           |       |      |
    --            Cons´           |       |      `Nil
    --                     field 1´       `field 2

Functions contain inner template `apply`. The template argument of `apply` is
the argument to the function and it should contain inner type `type` describing
the actual value. For example, this is not valid:

    add_ptr::type::apply<int>::type

Instead, the type `int` must be wrapped:

    template <typename T>
    struct wrap
    {
        typedef T type;
    };

    add_ptr::type::apply<wrap<int> >::type == int*

You can also create your own functions. See `add_ptr` above or look at the
built-in functions in `runtime/data.hpp`.

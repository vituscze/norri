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

This language takes this idea to the extreme. `norri` compiles a small
functional language with Haskell-like syntax into a templated C++ code that
can be compiled and "run" at compile time - much like the example above.

Syntax
------

This is a grammar for the language (parens and operator precedence omitted
for brevity):

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

    cons   ::= con "|" cons | con
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

    decls ::= value-def ";" decls | value-def

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

Norri is basically a lambda calculus with
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

Freshly generated code will contain references to `fix`, `__data` and
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
needed for correct implementation of user defined data types; it also contains
definitions of built-in types (`Int` and `Bool`) and various functions that
operate with values of these types.

To allow easy integration with existing metaprogramming code, the language
supports an abstract type `Type` which represents a C++ type and `assume`
construct which allows to bring in scope values that can be defined elsewhere.

For example, if we have operation `add_ptr` implemented on the C++ level:

    struct add_ptr
    {
        struct type
        {
            template <typename T>
            struct app
            {
                using type = typename T::type*;
            };
        };
    };

We can easily bring this operation in scope by writing:

    assume add_ptr : Type -> Type

Built-in functions
------------------

The language comes equipped with some predefined functions.

    neg   : Int -> Int
    plus  : Int -> Int -> Int
    minus : Int -> Int -> Int
    mul   : Int -> Int -> Int
    div   : Int -> Int -> Int
    rem   : Int -> Int -> Int
    eq    : Int -> Int -> Bool
    neq   : Int -> Int -> Bool
    lt    : Int -> Int -> Bool
    le    : Int -> Int -> Bool
    gt    : Int -> Int -> Bool
    ge    : Int -> Int -> Bool
    not_  : Bool -> Bool
    and_  : Bool -> Bool -> Bool
    or_   : Bool -> Bool -> Bool
    xor_  : Bool -> Bool -> Bool
    if_   : Bool -> a -> a -> a

Some of those functions also have their infix or prefix variant for more
readable code. Note that the pretty printer will always print their full name.

    prefix 7  ~   = neg
    prefix 7  !   = not
    infixl 6  *   = mul
    infixl 6  /   = div
    infixl 6  %   = rem
    infixl 5  +   = plus
    infixl 5  -   = minus
    infix  4  <   = lt
    infix  4  <=  = le
    infix  4  >   = gt
    infix  4  >=  = ge
    infix  4  ==  = eq
    infix  4  /=  = neq
    infixr 3  &&  = and_
    infixr 2  ^   = xor_
    infixr 1  ||  = or_

Usage
-----

The compiler is usually used with the following command:

    norri -o output input

This type checks the file `input` and if no error is found, it complies the
source into C++ code and writes it into `output`.

If `-a` is used, the user shoud also specify the location of the runtime using
`-i`:

    norri -a -i path/to/runtime -o output input

The resulting code can be used either directly (if the defined value is not
a function) - the computed type is available inside an inner type `type`.

    a : Int;
    a = 4

    -- Resulting C++ code.
    struct a { ... };

    a::type        == Int<4>
    a::type::value ==     4

User defined types are encoded in a more complex way:

    __data<constructor-number, field1, ..., fieldN>

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

    a::type == __data<1, Int<1>, __data<0>>
    --                ^  ^       ^      ^
    --                |  |       |      |
    --            Cons´  |       |      `Nil
    --           field 1´        `field 2

Functions contain inner template `app`. The template argument of `app` is
the argument to the function and it should contain inner type `type` describing
the actual value. For example, this is not valid:

    add_ptr::type::app<int>::type

Instead, the type `int` must be wrapped:

    template <typename T>
    struct wrap
    {
        using type = T;
    };

    add_ptr::type::app<wrap<int>>::type == int*
    // or
    apply<add_ptr, wrap<int>> == int*

You can also create your own functions. See `add_ptr` above or look at the
built-in functions in `runtime/data.hpp`.

Supported compilers
-------------------

So far, the following compilers have been tested and can successfully compile
the output of `norri`:

* gcc (any version with C++17 support)

* clang (any version with C++17 support)

Other compilers with correct support of C++17 might also work.

Examples
--------

Suppose we want to figure out greatest common divisor of few numbers and we'd
like to compute the answer at compile time (so that we can use it to declare
an array of that size, for example).

As a first step, we'll write the code in our language. To compute greatest
common divisor of two numbers we can use Euclid's algorithm:

    gcd : Int -> Int -> Int;
    gcd x y =
        -- Euclid's algorithm.
        let go a b = if_ (b == 0) a (go b (a % b))
        in  go (abs x) (abs y)

Absolute value of a number can be defined easily:

    abs : Int -> Int;
    abs n = if_ (n < 0) (~n) n

Now, we must define a list. Simple singly linked list will do:

    data List a = Nil | Cons a (List a)

This not only gives us constructors, but it also gives us an eliminator that
can be used to perform case analysis:

    Nil  : List a;
    Cons : a -> List a -> List a;

    list : z                   -- Case for empty list.
        -> (a -> List a -> z)  -- Case for nonempty list.
        -> List a              -- Input list.
        -> z                   -- Result.

With `list`, we can easily implement right fold:

    foldr : (a -> b -> b) -> b -> List a -> b;
    foldr f z = list z \x xs -> f x (foldr f z xs)

To compute greatest common divisor of a list of numbers, we take GCD of first
two, then GCD of the result and third number, and so on. The base case is 0,
since `gcd n 0 == n`:

    gcds = foldr gcd 0

Now we move to the C++ part. First, we compile the previous code:

    norri -o gcd.hpp gcd.nri

This gives us a structure named `gcds` which has inner template `type::app`
that can be used to compute the GCD. However, it takes the list in the
representation produced by compiler, which is rather verbose. To help with that,
we'll implement a helper template to convert from one representation to the
other.

Here's the encoding (see previous section):

    Nil       ==  __data<0>
    Cons a b  ==  __data<1, a, b>

Ideally, we'd like to simply write `ints_to_list<a, b, c, ...>`. This gives
us only once choice for `ints_to_list`:

    template <int... i>
    struct ints_to_list;

Now we have two cases to consider. If the argument list is empty, we'll simply
return `Nil`:

    template <>
    struct ints_to_list<>
    {
        using type = __data<0>;
    };

If the argument list contains at least one number, we must return `Cons`. First
field is simply the number. Second field is given by recursively using
`ints_to_list` on the rest of template arguments:

    template <int i, int... j>
    struct ints_to_list<i, j...>
    {
        using type = __data<1, Int<i>, typename ints_to_list<j...>::type>;
    };

Since `ints_to_list` already contains inner type `type`, we can directly use
it in the `app` template without needing any wrapper structure.

    gcds::type::app<ints_to_list<100, 80, 64>>::type::value
    // or
    apply<gcds, ints_to_list<100, 80, 64>>::value

* * *

It's also possible to use the computed values to initialize arrays. We'll start
with a function generating Fibonacci numbers.

    -- List as defined above.
    fib : Int -> List Int;
    fib =
        let go a b n = if_ (n <= 0) Nil (Cons a (go b (a + b) (n - 1)))
        in  go 0 1

    -- fib 5 == Cons 0 (Cons 1 (Cons 1 (Cons 2 (Cons 3 Nil))))

In the first example, we have a C++ template `ints_to_list` which converts a
variadic template into encoded list. Here, we need the other direction. Let us
start with a simple container for template parameters.

    template <typename...>
    struct pack
    { };

We can manipulate this container using template specialization. For this example,
the only operation we'll need is prepending a new element.

    template <typename, typename>
    struct add_front;

    template <typename T, typename... U>
    struct add_front<T, pack<U...>>
    {
        using type = pack<T, U...>;
    };

    // add_front<int, pack<char>> == pack<int, char>

Converting the encoded representation into this container is simply a matter of
recursively traversing the encoded list, prepending elements to the container
as we go.

    template <typename T>
    struct to_pack;

    // Nil case.
    template <>
    struct to_pack<__data<0>>
    {
        using type = pack<>;
    };

    // Cons case.
    template <typename T, typename U>
    struct to_pack<__data<1, T, U>>
    {
        // Recursively convert the tail and prepend the head.
        using type = typename add_front<T, typename to_pack<U>::type>::type;
    };

After the conversion, the container can be used to initialize an array using
the variadic template expansion.

    // Contains static array, initialized by the template parameter.
    template <typename>
    struct pack_to_array;

    template <typename... T>
    struct pack_to_array<pack<T...>>
    {
        static int array[];
    };

    template <typename... T>
    int pack_to_array<pack<T...>>::array[] = { T::value... };
    // T::value... extracts the constant named value from every template
    // parameter

After we compile the `fib` function, we can put all these templates together to
obtain an array initialized purely at compile time.

    using fibs = pack_to_array<to_pack<apply<fibs, Int<10>>>::type>;

    // ...

    for (int i = 0; i < 10; i++)
    {
        std::cout << fibs::array[i] << "\n";
    }

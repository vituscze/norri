#ifndef __NORRI_RUNTIME__BUILTIN
#define __NORRI_RUNTIME__BUILTIN

template <int i, typename...>
struct __data
{
    using type = __data;
    static constexpr int tag = i;
};

template <int i>
struct Int
{
    using type = Int;
    static constexpr int value = i;
};

template <bool b>
struct Bool
{
    using type = Bool;
    static constexpr bool value = b;
};

template <typename F, typename X>
using apply = typename F::type::template app<X>::type;

#define __unary(name, result) \
    struct name\
    {\
        struct type\
        {\
            template <typename A>\
            struct app\
            {\
                using type = result;\
            };\
        };\
    };

#define __binary(name, result) \
    struct name\
    {\
        struct type\
        {\
            template <typename A>\
            struct app\
            {\
                struct type\
                {\
                    template <typename B>\
                    struct app\
                    {\
                        using type = result;\
                    };\
                };\
            };\
        };\
    };

__unary(neg, Int<(-A::type::value)>)

__binary(plus,  Int<(A::type::value + B::type::value)>)
__binary(minus, Int<(A::type::value - B::type::value)>)
__binary(mul,   Int<(A::type::value * B::type::value)>)
__binary(div,   Int<(A::type::value / B::type::value)>)
__binary(rem,   Int<(A::type::value % B::type::value)>)

__binary(eq,  Bool<(A::type::value == B::type::value)>)
__binary(neq, Bool<(A::type::value != B::type::value)>)
__binary(lt,  Bool<(A::type::value <  B::type::value)>)
__binary(le,  Bool<(A::type::value <= B::type::value)>)
__binary(gt,  Bool<(A::type::value >  B::type::value)>)
__binary(ge,  Bool<(A::type::value >= B::type::value)>)

__unary(not_, Bool<(!A::type::value)>)

__binary(and_, Bool<(A::type::value && B::type::value)>)
__binary(or_,  Bool<(A::type::value || B::type::value)>)
__binary(xor_, Bool<(A::type::value ^  B::type::value)>)

struct if_
{
    struct type
    {
        template <typename A>
        struct app
        {
            struct type
            {
                template <typename B>
                struct app
                {
                    struct type
                    {
                        template <typename C>
                        struct app
                        {
                            template <bool b, typename __dummy>
                            struct __check;

                            template <typename __dummy>
                            struct __check<true, __dummy>
                            {
                                using type = typename B::type;
                            };

                            template <typename __dummy>
                            struct __check<false, __dummy>
                            {
                                using type = typename C::type;
                            };

                            using type = typename
                                __check<A::type::value, void>::type;
                        };
                    };
                };
            };
        };
    };
};

#endif /* __NORRI_RUNTIME__BUILTIN */

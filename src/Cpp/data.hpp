template <int i, typename dummy, typename...>
struct __data
{ };

struct dummy
{ };

template <int i>
struct Int
{
    static const int value = i;
};

template <bool b>
struct Bool
{
    static const bool value = b;
};

#define __unary(name, result) \
    struct name\
    {\
        struct type\
        {\
            template <typename A>\
            struct apply\
            {\
                typedef result type;\
            };\
        };\
    };

#define __binary(name, result) \
    struct name\
    {\
        struct type\
        {\
            template <typename A>\
            struct apply\
            {\
                struct type\
                {\
                    template <typename B>\
                    struct apply\
                    {\
                        typedef result type;\
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
        struct apply
        {
            struct type
            {
                template <typename B>
                struct apply
                {
                    struct type
                    {
                        template <typename C>
                        struct apply
                        {
                            template <bool b, typename dummy>
                            struct __check;

                            template <typename dummy>
                            struct __check<true, dummy>
                            {
                                typedef typename B::type type;
                            };

                            template <typename dummy>
                            struct __check<false, dummy>
                            {
                                typedef typename C::type type;
                            };

                            typedef typename
                                __check<A::type::value, dummy>::type type;
                        };
                    };
                };
            };
        };
    };
};

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

#define unary(name, result) \
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

#define binary(name, result) \
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

unary(neg, Int<(-A::type::value)>)

binary(plus,  Int<(A::type::value + B::type::value)>)
binary(minus, Int<(A::type::value - B::type::value)>)
binary(mul,   Int<(A::type::value * B::type::value)>)
binary(div,   Int<(A::type::value / B::type::value)>)
binary(rem,   Int<(A::type::value % B::type::value)>)

binary(eq,  Bool<(A::type::value == B::type::value)>)
binary(neq, Bool<(A::type::value != B::type::value)>)
binary(lt,  Bool<(A::type::value <  B::type::value)>)
binary(le,  Bool<(A::type::value <= B::type::value)>)
binary(gt,  Bool<(A::type::value >  B::type::value)>)
binary(ge,  Bool<(A::type::value >= B::type::value)>)

unary(not_, Bool<(!A::type::value)>)

binary(and_, Bool<(A::type::value && B::type::value)>)
binary(or_,  Bool<(A::type::value || B::type::value)>)
binary(xor_, Bool<(A::type::value ^  B::type::value)>)

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

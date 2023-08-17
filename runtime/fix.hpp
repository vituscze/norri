#ifndef __NORRI_RUNTIME__FIX
#define __NORRI_RUNTIME__FIX

struct fix
{
    struct type
    {
        template <typename F>
        struct app
        {
            using type = typename F::type::template app<app<F>>::type;
        };
    };
};

#endif /* __NORRI_RUNTIME__FIX */

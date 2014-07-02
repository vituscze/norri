#ifndef __TMPCOMPILER_RUNTIME__FIX
#define __TMPCOMPILER_RUNTIME__FIX

struct fix
{
    struct type
    {
        template <typename f, int i>
        struct apply_rec
        {
            typedef typename
                f::type::template apply<apply_rec<f, i + 1> >::type type;
        };

        template <typename f>
        struct apply
        {
            typedef typename apply_rec<f, 0>::type type;
        };
    };
};

#endif /* __TMPCOMPILER_RUNTIME__FIX */

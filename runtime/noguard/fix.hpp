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

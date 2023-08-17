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

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

#include "a.hpp"

template <typename T>
struct wrap { using type = T; };
       
int main() { apply<apply<twice, add_ptr>, wrap<int>> x = 1; }

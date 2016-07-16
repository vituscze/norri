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

#include "a.hpp"

template <typename T>
struct wrap { typedef T type; };
       
int main() { twice::type::apply<add_ptr>::type::apply<wrap<int>>::type x = 1; }

// To use this file, compile 'pointer.nri' with
// norri -o pointer.hpp pointer.nri
#include <iostream>
#include <type_traits>

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

#include "pointer.hpp"

template <typename T>
struct wrap
{
    using type = T;
};

int main()
{
    std::cout << "is same: " << std::boolalpha
        << std::is_same_v<int**, apply<add_ptr_2, wrap<int>>>
        << "\n";
}

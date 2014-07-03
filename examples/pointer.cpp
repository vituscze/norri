// To use this file, compile 'pointer.tmpc' with
// tmpcompiler -o pointer.hpp pointer.tmpc
#include <iostream>
#include <type_traits>

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

#include "pointer.hpp"

template <typename T>
struct wrap
{
    typedef T type;
};

int main()
{
    std::cout << "is same: " << std::boolalpha
        << std::is_same<int**, add_ptr_2::type::apply<wrap<int> >::type>::value
        << "\n";
}

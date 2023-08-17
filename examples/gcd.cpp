// To use this file, compile 'gcd.nri' with
// norri -o gcd.hpp gcd.nri
#include <iostream>

#include "gcd.hpp"

// Convert a template argument list (of ints) to a List as compiled by
// norri.
template <int... i>
struct ints_to_list;

template <>
struct ints_to_list<>
{
    using type = __data<0>;
};

template <int i, int ... j>
struct ints_to_list<i, j...>
{
    using type = __data<1, Int<i>, typename ints_to_list<j...>::type>;
};

int main()
{
    std::cout << "gcd(100, 80, 64) = "
        << apply<gcds, ints_to_list<100, 80, 64>>::value
        << "\n";
}

// To use this file, compile 'gcd.nri' with
// norri -o gcd.hpp gcd.nri
#include <iostream>

#include "gcd.hpp"

// Convert a template argument list (of ints) to a List as compiled by
// norri.
template <int ... i>
struct ints_to_list;

template <>
struct ints_to_list<>
{
    typedef __data<0, __dummy> type;
};

template <int i, int ... j>
struct ints_to_list<i, j...>
{
    typedef __data<1, __dummy, Int<i>, typename ints_to_list<j...>::type> type;
};

int main()
{
    std::cout << "gcd(100, 80, 64) = "
        << gcds::type::apply<ints_to_list<100, 80, 64> >::type::value
        << "\n";
}

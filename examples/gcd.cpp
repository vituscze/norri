// To use this file, compile 'gcd.tmpc' with
// tmpcompiler -o gcd.hpp gcd.tmpc
#include <iostream>

#include "gcd.hpp"

template <typename T>
struct wrap
{
    typedef T type;
};

// Convert a template argument list (of ints) to a List as compiled by
// tmpcompiler.
template <int ... I>
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

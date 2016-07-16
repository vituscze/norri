#include "a.hpp"

template <int ... I>
struct ints_to_list;

template <>
struct ints_to_list<>
{
    typedef __data<1, __dummy> type;
};

template <int I, int ... J>
struct ints_to_list<I, J...>
{
    typedef __data<0, __dummy, Int<I>, typename ints_to_list<J...>::type> type;
};
       
int main() { gcds::type::apply<ints_to_list<81, 45, 120>>::type x = 0; }

#include "a.hpp"

template <int ... I>
struct ints_to_list;

template <>
struct ints_to_list<>
{
    using type = __data<1>;
};

template <int I, int ... J>
struct ints_to_list<I, J...>
{
    using type = __data<0, Int<I>, typename ints_to_list<J...>::type>;
};
       
int main() { apply<gcds, ints_to_list<81, 45, 120>> x = 0; }

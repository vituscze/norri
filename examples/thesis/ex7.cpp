#include <iostream>

#include "a.hpp"

template <int... I>
struct int_list { };

template <int I, typename T>
struct add_front;

template <int I, int... J>
struct add_front<I, int_list<J...>> {
    using type = int_list<I, J...>;
};

template <typename T>
struct simplify;

template <int I, typename Rest>
struct simplify<__data<0, Int<I>, Rest>> {
    using type = typename add_front<I, typename simplify<Rest>::type>::type;
};

template <>
struct simplify<__data<1>> {
    using type = int_list<>;
};

template <typename T>
struct array_init;

template <int... I>
struct array_init<int_list<I...>> {
    static int array[];
};

template <int... I>
int array_init<int_list<I...>>::array[] = {I...};

using result_array = array_init<simplify<result::type>::type>;
       
int main() {
    for (int i = 0; i < 3; ++i) {
        std::cout << result_array::array[i] << '\n';
    }
}

#include <iostream>

#include "a.hpp"

template <int... I>
struct int_list { };

template <int I, typename T>
struct add_front;

template <int I, int... J>
struct add_front<I, int_list<J...>> {
    typedef int_list<I, J...> type;
};

template <typename T>
struct simplify;

template <int I, typename Dummy, typename Rest>
struct simplify<__data<0, Dummy, Int<I>, Rest>> {
    typedef typename add_front<I, typename simplify<Rest>::type>::type type;
};

template <typename Dummy>
struct simplify<__data<1, Dummy>> {
    typedef int_list<> type;
};

template <typename T>
struct array_init;

template <int... I>
struct array_init<int_list<I...>> {
    static int array[];
};

template <int... I>
int array_init<int_list<I...>>::array[] = {I...};

typedef array_init<simplify<result::type>::type> result_array;
       
int main() {
    for (int i = 0; i < 3; ++i) {
        std::cout << result_array::array[i] << '\n';
    }
}

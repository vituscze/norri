// To use this file, compile 'fib.nri' with
// norri -o fib.hpp fib.nri
#include <iostream>

#include "fib.hpp"

// Template parameter pack.
template <typename ...>
struct pack
{ };

// Adding new type to the front.
template <typename, typename>
struct add_front;

template <typename T, typename ... U>
struct add_front<T, pack<U...> >
{
    typedef pack<T, U...> type;
};

// Convert a 'List' to a pack.
template <typename T>
struct to_pack;

template <typename dummy>
struct to_pack<__data<0, dummy> >
{
    typedef pack<> type;
};

template <typename T, typename dummy, typename U>
struct to_pack<__data<1, dummy, T, U> >
{
    typedef typename add_front<T, typename to_pack<U>::type>::type type;
};

// Simple wrapping struct for inner 'type'.
template <typename T>
struct wrap
{
    typedef T type;
};

// Use the 'pack' to initialize an array.
template <typename>
struct pack_to_array;

template <typename ... T>
struct pack_to_array<pack<T...> >
{
    static int array[];
};

template <typename ... T>
int pack_to_array<pack<T...> >::array[] = { T::value... };

int main()
{
    typedef
        pack_to_array<
            to_pack<
                fib::type::apply<
                    wrap<Int<10> >
                >::type
            >::type
        > fibs;

    for (int i = 0; i < 10; i++)
    {
        std::cout << fibs::array[i] << "\n";
    }
}

// To use this file, compile 'fib.nri' with
// norri -o fib.hpp fib.nri
#include <iostream>

#include "fib.hpp"

// Template parameter pack.
template <typename...>
struct pack
{ };

// Adding new type to the front.
template <typename, typename>
struct add_front;

template <typename T, typename... U>
struct add_front<T, pack<U...>>
{
    using type = pack<T, U...>;
};

// Convert a 'List' to a pack.
template <typename T>
struct to_pack;

template <>
struct to_pack<__data<0>>
{
    using type = pack<>;
};

template <typename T, typename U>
struct to_pack<__data<1, T, U>>
{
    using type = typename add_front<T, typename to_pack<U>::type>::type;
};

// Use the 'pack' to initialize an array.
template <typename>
struct pack_to_array;

template <typename... T>
struct pack_to_array<pack<T...>>
{
    static int array[];
};

template <typename... T>
int pack_to_array<pack<T...>>::array[] = { T::value... };

int main()
{
    using fibs = pack_to_array<to_pack<apply<fib, Int<10>>>::type>;

    for (int i = 0; i < 10; i++)
    {
        std::cout << fibs::array[i] << "\n";
    }
}

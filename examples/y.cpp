// To use this file, compile 'y.nri' with
// norri -o y.hpp y.nri
#include <iostream>

#include "y.hpp"

template <typename T>
struct wrap
{
    typedef T type;
};

int main()
{
    std::cout << "fac(10) == "
      << fac::type::apply<wrap<Int<10> > >::type::value
      << "\n";
}

// To use this file, compile 'y.tmpc' with
// tmpcompiler -o y.hpp y.tmpc
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

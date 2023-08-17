// To use this file, compile 'y.nri' with
// norri -o y.hpp y.nri
#include <iostream>

#include "y.hpp"

int main()
{
    std::cout << "fac(10) == "
      << apply<fac, Int<10>>::value
      << "\n";
}

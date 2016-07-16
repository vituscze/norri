#include "a.hpp"
   
template <typename T>
struct wrap { typedef T type; };   
       
int main() { id::type::apply<wrap<Int<2>>>::type x = 0; }

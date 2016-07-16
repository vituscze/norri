#include "a.hpp"
   
template <typename T>
struct wrap
{
    typedef T type;
};   
   
int main() { odd::type::apply<wrap<Int<1>>>::type x = 0; }

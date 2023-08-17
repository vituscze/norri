#include "runtime/data.hpp"

struct fail
{
    template <typename T>
    struct report_error
    {
        static_assert(T::value, "fail encountered");            
    };
    
    using type = report_error<Bool<false>>;
};

#include "a.hpp"
   
int main() { result_ok::type x = 0; }

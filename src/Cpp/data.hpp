template <int i, typename dummy, typename...>
struct __data
{ };

struct dummy
{ };

template <int i>
struct Int
{
    static const int value = i;
};

template <bool b>
{
    static const bool value = b;
};

#include <iostream>

// Bit Twiddling Functions
//
// Algorithms presented here are from Hacker's Delight.

namespace bits {

template <typename T>
bool is_set(T const& num, std::size_t which)
{
  return num & (1 << which);
}


template <typename T>
T set(T const& num, std::size_t which)
{
  return num | (1 << which);
}

template <typename T>
T clear(T const& num, std::size_t which)
{
  return num & ~(1 << which);
}

template <typename T>
T toggle(T const& num, std::size_t which)
{
  return num ^ (1 << which);
}

template <typename T>
T low(T const& num)
{
  return num & -num;
}

template <typename T>
T set_all(T const& num, std::size_t which)
{
  return (1 << which) - 1;
}

template <typename T>
T modulo(T const& x, std::size_t n)
{
  return x & (n - 1);
}

template <typename T>
bool is_power_of_2(T const& num)
{
  return num & (num - 1) == 0;
}

} // namespace bits


int main()
{
}

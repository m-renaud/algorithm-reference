#include "gcd.hxx"

// Least Common Multiple

template <typename T>
T lcm(T const& a, T const& b)
{
  return (a*b)/gcd(a,b);
}

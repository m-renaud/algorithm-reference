#include <iostream>

#include "fast_exponent.hxx"

int main()
{
  double base;
  int exponent;

  std::cin >> base >> exponent;

  std::cout << fast_exponent(base,exponent) << std::endl;
}

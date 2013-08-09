#include <iostream>
#include "sum_combinations.hxx"

int main()
{
  sum_combination sc;
  long long n;

  std::cin >> n;

  std::cout << sc(n) << std::endl;
  return 0;
}

#include <iostream>
#include "../gcd.hxx"

int main()
{
  for (int m, n; std::cin >> m >> n; )
    std::cout << gcd(m, n) << std::endl;
}

#include <array>
#include <iostream>
#include "line_intersection.hxx"

int main()
{
  double a, b, c, d, e, f;

  std::cin >> a >> b >> c >> d >> e >> f;
  std::array<double,2> poi = intersection_point(a,b,c,d,e,f);
  std::cout << "(" << poi[0] << " " << poi[1] << ")" << std::endl;
}

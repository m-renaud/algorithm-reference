#include <array>

//
// Determine which side of a line a point px,py lies on.
// Input:
//   Line  :: Directed from (x1,y1) through (x2,y2)
//   Point :: At (px, py)
//
// Output:
//   -1 :: Right of line.
//    1 :: Left of line.
//    0 :: On line.
//
template<typename T>
char pointSide(T x1, T y1, T x2, T y2, T px, T py)
{
  T d = (x2-x1) * (py-y1) - (px-x1) * (y2-y1);

  if (d == 0)
    return 0;
  else if (d > 0)
    return 1;
  else
    return -1;
}

template <typename T>
bool point_on_left(T x1, T y1, T x2, T y2, T px, T py)
{
  return (x2-x1) * (py-y1) > (px-x1) * (y2-y1);
}

template <typename T>
bool intersect(T x1, T y1, T x2, T y2, T x3, T y3, T x4, T y4)
{
  bool a = (x2-x1)*(y3-y1) > (x3-x1)*(y2-y1);
  //   a = point_on_left(x1, y1, x2, y2, x3, y3);
  bool b = (x2-x1)*(y4-y1) > (x4-x1)*(y2-y1);
  //   b = point_on_left(x1, y1, x2, y2, x4, y4);
  bool c = (x4-x3)*(y1-y3) > (x1-x3)*(y4-y3);
  //   c = point_on_left(x3, y3, x4, y4, x1, y1);
  bool d = (x4-x3)*(y2-y3) > (x2-x3)*(y4-y3);
  //   d = point_on_left(x3, y3, x4, y4, x2, y2);

  return (a !=b) && (c != d);
}

template <typename T>
T slope(T x1, T y1, T x2, T y2)
{
  return (y2-y1) / (x2-x1);
}



template <typename T>
std::array<T,2> intersection_point(T a, T b, T c, T d, T e, T f)
{
  std::array<T,2> poi;

  const double eps = 0.0000000001;
  double det_denom = a*e - b*d;

  if(abs(det_denom) < eps) // ae == bd
  {
    std::cout << "SHIT\n";
  }
  else
  {
    double det = 1 / det_denom;
    e *= det;
    b *= -det;
    d *= -det;
    a *= det;
    c *= -1;
    f *= -1;
    poi[0] = e*c + b*f;
    poi[1] = d*c + a*f;
  }

  return poi;
}

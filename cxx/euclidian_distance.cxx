#include <iostream>
#include <initializer_list>
#include <utility>
#include <vector>

#include <cmath>


template <typename T>
T square(T const& x)
{
  return x * x;
}


// Compute the distance between two points in 2-dimensional space.
template<typename T>
double dist(std::pair<T,T> const& p1, std::pair<T,T> const& p2)
{
  return sqrt(square(p2.first - p1.first) + square(p2.second - p1.second));
}


// Compute the distance between two points in n-dimentional space.
// [aBeg, aEnd) specify the first point, and the range starting
// at bBeg must be of the same length.
//
// double is used as the return type to force it to be a floating
// point type.
template <typename Iter>
double dist(Iter aBeg, Iter aEnd, Iter bBeg)
{
	double total;

	for (; aBeg != aEnd; ++aBeg, ++bBeg)
		total += square(*bBeg - *aBeg);

	return sqrt(total);
}


// Compute the distance between two points in n-dimensional space,
// given by the contents of two sequence containers.
template <typename Container>
double dist(Container const& c1, Container const& c2)
{
	return dist(c1.begin(), c1.end(), c2.begin());
}


// Overload taking an initializer list, just for fun.
template <typename T>
double dist(std::initializer_list<T> i, std::initializer_list<T> j)
{
	return dist(i.begin(), i.end(), j.begin());
}


int main()
{
	std::cout << dist(std::make_pair(0,0), std::make_pair(5,5)) << std::endl;
	std::cout << dist({0,0}, {5,5}) << std::endl;
}

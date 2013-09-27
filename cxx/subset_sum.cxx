#include <numeric>
#include <iostream>
#include <iterator>
#include <vector>

// Determine if you can take a subset of elements that sum to k.
//
// The return value is a vector where the kth element is 0 or 1.
// 0 if a subset CAN NOT sum to k.
// 1 if a subset CAN sum to k;
//
// M == sum of all values in the sequence.
//
// Base Case: Sum of 0 can always be achieved.
//
// Loop through each element (begin) in the sequence and each j from
// M down to *begin. If you can create a sum of (j - *begin), than you
// can create a sum of j by including the current element *begin.
template <typename Iter>
std::vector<int> subset_sum(Iter begin, Iter end)
{
	using value_type = typename Iter::value_type;

	value_type M = std::accumulate(begin, end, 0);

	std::vector<int> m(M + 1);
	m[0] = 1;

	for (; begin != end; ++begin)
		for (int j = M; j >= *begin; --j)
			m[j] |= m[j - *begin];

	return m;
}


// Determine if you can take a subset of elements that sum to k, while
// being allowed to repeat elements in the sequence an arbitrary number
// of times.
//
// The return value is a vector where the kth element is 0 or 1.
// 0 if a subset CAN NOT sum to k.
// 1 if a subset CAN sum to k;
//
// M == sum of all values in the sequence.
//
// Base Case: Sum of 0 can always be achieved.
//
// Loop through each element (begin) in the sequence and each j from
// *begin up to M. If you can create a sum of (j - *begin), than you
// can create a sum of j by including the current element *begin.
//
// The order is reversed to ...
template <typename Iter>
std::vector<int> subset_sum_multiple_suppplies(Iter begin, Iter end)
{
	using value_type = typename Iter::value_type;
	value_type M = std::accumulate(begin, end, 0);

	std::vector<int> m(M + 1);
	m[0] = 1;

	for (; begin != end; ++begin)
		for (int j = *begin; j <= M; ++j)
			m[j] |= m[j - *begin];

	return m;
}

int main()
{
	std::vector<int> a(
		std::istream_iterator<int>(std::cin),
		std::istream_iterator<int>()
	);

	std::vector<int> m = subset_sum(a.begin(), a.end());

	for (auto i = m.begin(); i != m.end(); ++i)
		std::cout << *i << " ";
	endl(std::cout);

	m = subset_sum_multiple_suppplies(a.begin(), a.end());

	for (auto i = m.begin(); i != m.end(); ++i)
		std::cout << *i << " ";
	endl(std::cout);
}

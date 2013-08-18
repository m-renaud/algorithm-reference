#include <iostream>
#include <iterator>
#include <utility>
#include <vector>

// Maximum value contiguous subsequence.
//
// Given a sequence denoted by [begin,end), find the contiguous
// subsequence that has the "maximum" value as determined by the
// comparator (std::greater by default).
//
// For example, given the sequence: [-10, 3, -14, 8, 1, -2 5 ]
// The maximum range is [ 8, 1, -2, 5 ] with a value of 12.
//
// Algorithm:
//
//   1) The first element is the maximum range.
//
//   2) For each element x:
//
//     a) Determine if appending x to the current sequence S is better
//        than starting a new sequence containing only x.
//
//     b) If the current sequence has a larger sum than the current maximum,
//        update the current maximum.
//

template <
	typename Iter,
	typename Comp = std::greater<typename Iter::value_type>
>
auto maximum_contiguous_subsequence(Iter begin, Iter end, Comp comp = Comp())
	-> std::pair<Iter,Iter>
{
	using value_type = typename Iter::value_type;

	if (begin == end)
		return std::make_pair(begin, end);

	// Range to store the begin and end of the maximum subsequence.
	std::pair<Iter, Iter> range = std::make_pair(begin, std::next(begin));
	value_type max_value = *begin;

	Iter current_begin = begin;
	value_type current_value = *begin;

	for (++begin; begin != end; ++begin)
	{
		// We get a better subsequence by including this element.
		if (comp(current_value + *begin, *begin))
		{
			current_value += *begin;
		}
		// Better to just take this element in isolation.
		else
		{
			current_value = *begin;
			current_begin = begin;
		}

		// Check if the current sequence is better than our best seuqence.
		if (comp(current_value, max_value))
		{
			max_value = current_value;
			range = std::make_pair(current_begin, std::next(begin));
		}
	}

	return range;
}


int main()
{
	std::cout << "Enter a sequence of integers: ";

	std::vector<int> A(
		std::istream_iterator<int>(std::cin),
		std::istream_iterator<int>()
	);

	auto range = maximum_contiguous_subsequence(A.begin(), A.end());

	int sum = 0;

	for (auto i = range.first; i != range.second; ++i)
	{
		sum += *i;
		std::cout << *i << " ";
	}
	endl(std::cout);

	std::cout << "Sum = " << sum << std::endl;

}

#include <iostream>
#include <iterator>
#include <utility>
#include <vector>

// Maximum value contiguous subsequence.
//
// Given a sequence denoted by [begin,end), find the subsequenc
// that has the "maximum" value as determined by the comparator.

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

	// Iterators to denote the begin and end of the range of
	// maximum subsequence.
	std::pair<Iter, Iter> range = std::make_pair(begin, std::next(begin));

	Iter range_begin = begin;
	Iter range_end = std::next(begin);

	// The maximum value is the first element to start.
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

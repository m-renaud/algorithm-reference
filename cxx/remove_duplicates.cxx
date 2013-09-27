#include <iostream>
#include <forward_list>

// Remove duplicates elements from a range.
// Follows the convention of algorithms in <algorithm>, moving all
// items to be *deleted* to the end of the range and returning an
// iterator to the element past the end of the valid range.
//
// Note: The range must already be sorted for this algorithm to apply.
template <typename Iter>
Iter remove_duplicates(Iter first, Iter last)
{
	if (first == last)
		return last;

	Iter prev, new_last;

	prev = first;
	std::advance(first,1);
	new_last = first;

	for (; first != last; ++first, ++prev)
		if (*first != *prev)
			*new_last++ = *first;

	return new_last;
}

int main()
{
	std::forward_list<int> numbers = {1,2,3,3,4,4,5,6,6,6,7,7};

	auto e = remove_duplicates(numbers.begin(), numbers.end());
	for (auto i = numbers.begin(); i != e; ++i)
		std::cout << *i << ((std::next(i) != e) ? ", " : "\n");

	return 0;
}

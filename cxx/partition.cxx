#include <iostream>
#include <vector>


template <typename Iter>
Iter partition(Iter first, Iter last, typename Iter::value_type const& x)
{
	using std::swap;

	Iter insert = first;
	while (insert != last && *insert < x)
		++insert;

	for (first = std::next(insert); first != last; ++first)
	{
		if (*first < x)
		{
			swap(*first, *insert);
			++insert;
		}
	}

	return insert;
}




int main()
{
	std::vector<int> vec {1,8,3,4,7,6,5,18};

	auto mid = partition(vec.begin(), vec.end(), 5);

	std::cout << "Less: ";
	for (auto i = vec.begin(); i != mid; ++i)
		std::cout << *i << " ";
	endl(std::cout);

	std::cout << "More: ";
	for (auto i = mid; i != vec.end(); ++i)
		std::cout << *i << " ";
	endl(std::cout);
}

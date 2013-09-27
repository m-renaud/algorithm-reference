#include <iostream>
#include <list>
#include <string>
#include <iterator>
#include <vector>


template <typename Iter>
void merge_sort(Iter beg, Iter end);

template <typename Iter, typename SizeType>
void merge_sort(Iter beg, Iter end, SizeType sz);

template <typename Iter>
void merge(Iter bLeft, Iter bRight, Iter end);

int main()
{
	std::vector<int> vec {1,2,3,2,5,4,6,5,43,7,6};

	merge_sort(vec.begin(), vec.end(), std::distance(vec.begin(), vec.end()));

	for (int x : vec)
		std::cout << x << ' ';
	endl(std::cout);
}


template <typename Iter>
void merge_sort(Iter beg, Iter end)
{
	merge_sort(beg, end, std::distance(beg, end));
}


template <typename Iter, typename SizeType>
void merge_sort(Iter beg, Iter end, SizeType sz)
{
	if (sz <= 1)
		return;

	auto half_size = sz/2;
	Iter middle = beg;
	std::advance(middle, half_size);

	merge_sort(beg, middle, half_size);
	merge_sort(middle, end, sz - half_size);

	merge(beg, middle, end);
}


template <typename Iter>
void merge(Iter bLeft, Iter bRight, Iter end)
{
	std::vector<typename Iter::value_type> temp;
	Iter cur_left = bLeft;
	Iter cur_right = bRight;

	while (cur_left != bRight && cur_right != end)
		temp.push_back((*cur_left < *cur_right) ? *cur_left++ : *cur_right++);

	while (cur_left != bRight)
		temp.push_back(*cur_left++);

	while (cur_right != end)
		temp.push_back(*cur_right++);

	for (Iter cur = temp.begin(); cur != temp.end(); ++cur)
		*bLeft++ = *cur;
}

#include <iostream>
#include <iterator>
#include <set>
#include <vector>

// Longest Increasing Subsequence


//=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
// Find the longest increasing subsequence (l) given a sequence (m).
//
// Complexity:
//   Time: O(n log n)
//   Space: O(l)
template <typename Iter>
std::size_t longest_increasing_sequence_set(Iter first, Iter last)
{
	std::set<typename Iter::value_type> seq;

	for (; first != last; ++first)
	{
		auto it = ++(seq.insert(*first).first);

		if (it != seq.end())
			seq.erase(it);
	}

	return seq.size();
}


int main()
{
	std::vector<int> vec(
		std::istream_iterator<int>(std::cin),
		std::istream_iterator<int>()
	);

	std::cout << longest_increasing_sequence_set(vec.begin(), vec.end()) << std::endl;
}

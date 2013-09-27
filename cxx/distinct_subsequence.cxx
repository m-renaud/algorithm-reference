#include <iostream>
#include <string>
#include <map>
#include <vector>

// Number of Distinct Subsequences


//m=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
template <typename Iter>
unsigned long long distinct_subsequences(Iter begin, Iter end)
{
	using value_type = typename std::iterator_traits<Iter>::value_type;
	using map_type = std::map<value_type,int>;
	using map_iter = typename map_type::iterator;
	using Int = unsigned long long;

	std::vector<Int> sum;
	map_type last_occurence;

	// Reserve space for n elements.
	sum.reserve(std::distance(begin, end));

	// Empty string has 1 subsequence.
	sum.push_back(1);

	for (; begin != end; ++begin)
	{
		// Number of subsequences without this character, minus the number of
		// subsequences we had last time we encountered the character.
		// Adding the same character does not produce a new subsequence

		// Get an iterator to the pair<value_type, size_type> of the last
		// position where the current element occurred.
		map_iter last = last_occurence.find(*begin);

		// If it was not seen before, there are no repeats.
		// If it has been, number of repeats is the sum just before we saw
		// that character.
		Int repeats = (last == last_occurence.end() ? 0 : sum[last->second]);

		Int new_subsequences = sum.back() - repeats;

		// Add the new subsequences to the old sum
		sum.push_back(sum.back() + new_subsequences);

		// Record where we last saw this character.
		// (Index into the sum array before we included this character).
		last_occurence[*begin] = sum.size() - 2;
	}

	return sum.back();
}


int main()
{
	std::string input;

	while (std::cin >> input)
		std::cout << distinct_subsequences(begin(input), end(input)) << std::endl;

	return 0;
}

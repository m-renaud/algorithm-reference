#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

// Levenshtein Distance
//
// The number of modifications (insert, modify, delete) required for
// the first sequence to be equal to the second.

template <typename Iter1, typename Iter2>
std::size_t levenshtein(Iter1 first1, Iter1 last1, Iter2 first2, Iter2 last2)
{
	using value_type = typename Iter1::value_type;
	using difference_type = typename Iter1::difference_type;

	// If a ranges is empty, the distance is the length of the other.
	if (first1 == last1)
		return std::distance(first2, last2);
	if (first2 == last2)
		return std::distance(first1, last1);

	const difference_type length1 = std::distance(first1, last1);
	const difference_type length2 = std::distance(first2, last2);

	std::vector<std::size_t> row(length2 + 1);

	// Initialize the first row of the table.
	for (std::size_t i = 0; i <= length2; ++i)
		row[i] = i;

	Iter1 pos1 = first1;
	Iter2 pos2;

	// Compute the distance between the ranges.
	for (std::size_t i = 1; i <= length1; ++i, ++pos1)
	{
		row[0] = i;
		std::size_t corner = i - 1;
		pos2 = first2;

		for (std::size_t j = 1; j <= length2; ++j, ++pos2)
		{
			std::size_t upper = row[j];

			if (*pos1 == *pos2)
				row[j] = corner;
			else
				row[j] = std::min(row[j-1], std::min(upper, corner)) + 1;

			corner = upper;
		}
	}

	return row[length2];
}


// Compute levenschtein between two sequence containers.
template <typename C1, typename C2>
std::size_t levenshtein(C1 const& c1, C2 const& c2)
{
	using std::begin;
	using std::end;

	return levenshtein(
		begin(c1), end(c1),
		begin(c2), end(c2)
	);
}


int main()
{
	std::string s1 = "kitchen";
	std::string s2 = "stitching";
	std::cout << levenshtein(s1.begin(), s1.end(), s2.begin(), s2.end());
	std::cout << std::endl;
}

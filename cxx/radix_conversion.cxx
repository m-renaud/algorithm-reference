#include <algorithm>
#include <iostream>
#include <string>

// Radix Conversion

template <typename IntType, typename ResultType = std::string>
ResultType to_radix(IntType n, IntType r)
{
	const static char digit_list[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
	ResultType result;

	for (; n!=0; n/=r)
		result.push_back(digit_list[n%r]);

	std::reverse(result.begin(), result.end());
	return result;
}


int main()
{
	auto base_14 = to_radix(1234, 14);
	std::cout << base_14 << std::endl;
}

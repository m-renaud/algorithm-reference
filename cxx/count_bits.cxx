#include <iostream>
#include <unordered_map>

// Count the number of bits in a number.
//
// The cache is somewhat overkill if you're dealing with types with
// a small number of bits and can also become incredibly space consuming.
// A more ideal approach is to use the lg time algorithm for large bit
// sets and only cache the number of bits in small numbers.
//
struct pop
{
	unsigned operator()(unsigned n)
	{
		static std::unordered_map<unsigned,unsigned> cache;

		auto iter = cache.find(n);
		if (iter != end(cache))
			return iter->second;

		unsigned count = 0;
		for (; n; ++count)
			n &= n-1;
		cache[n] = count;

		return count;
	}

};


int main()
{
	pop bit_counter;
	unsigned n;
	std::cout << "Enter an integer: " ;
	std::cin >> n;
	std::cout << bit_counter(n) << std::endl;
}

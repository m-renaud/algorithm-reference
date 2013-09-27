#include <iostream>
#include <unistd.h>

template <typename BidiIter>
void reverse(BidiIter begin, BidiIter end)
{
	while ((begin != end) && (begin != --end))
		std::iter_swap(begin++, end);
}



int main()
{
	std::string s1("hello there");
	reverse(std::next(s1.begin()), s1.end());

	std::cout << s1 << std::endl;
}

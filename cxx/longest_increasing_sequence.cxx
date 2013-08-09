#include <iostream>
#include <utility>
#include <vector>

// Longest Increasing Sequence


template <typename Iter, typename Comp>
std::size_t longest_increasing_sequence(
  Iter first, Iter last,
  Comp comp = std::greater<typename Iter::value_type>
)
{
  if (first == last)
    return 0;

  std::size_t max = 1;
  std::size_t length = 0;

  for (Iter next = std::next(first); next != last; ++first, ++next)
  {
    if (comp(*next, *first))
    {
      ++length;
      if (length > max)
    }
    else
    {

    }
  }

  return max;
}


template <typename Container>
inline std::size_t longest_increasing_sequence(Container const& v)
{
  return longest_increasing_sequence_set(v.begin(), v.end());
}




int main()
{
  std::vector<int> v {1, 2, 3, 2, 3, 4, 5};

  std::cout << longest_increasing_sequence_set(v.begin(), v.end())
            << std::endl;
}

#include <algorithm>
#include <iostream>
#include <vector>

// Longest Common Subsequence


//=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
template <
  typename Iter1, typename Iter2,
  template <typename...> class Container = std::vector
>
Container<typename Iter1::value_type> longest_common_subsequence(
  Iter1 first1, Iter1 last1, Iter2 first2, Iter2 last2)
{
  using value_type = typename Iter1::value_type;

  int m = std::distance(first1, last1);
  int n = std::distance(first2, last2);

  // If a sequence is empty, return empty container.
  if (m == 0 || n == 0)
    return Container<value_type>();

  std::vector<std::vector<int> > table(
    m + 1,
    std::vector<int>(n + 1, 0)
  );

  Iter2 y;

  for (int i = 1; i < m + 1; ++first1, ++i)
  {
    y = first2;

    for (int j = 1; j < n + 1; ++y, ++j)
    {
      if (*first1 == *y)
        table[i][j] = table[i-1][j-1] + 1;
      else
        table[i][j] = std::max(table[i-1][j], table[i][j-1]);
    }
  }

  //m--------------------------------------------------
  // Backtrace to find one of the LCSs.
  //

  // Move iterators back to last valid element.
  --last1;
  --last2;

  Container<value_type> output;

  // Go backwards through the sequence.
  while (m >= 0 && n >= 0)
  {
    // If the characters are the same, add it to the sequence.
    if (*last1 == *last2)
    {
      output.push_back(*last1);
      --last1;
      --last2;
      --m;
      --n;
    }

    // If different, take the maximum of the two sequences.
    else if (table[m][n-1] > table[m-1][n])
    {
      --last2;
      --n;
    }
    else
    {
      --last1;
      --m;
    }
  }

  std::reverse(output.begin(), output.end());

  return output;
}


//=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
template <typename Iter1, typename Iter2, typename Iter3>
int longest_common_subsequence(
  Iter1 first1, Iter1 last1,
  Iter2 first2, Iter2 last2,
  Iter2 first3, Iter2 last3
)
{
  typedef typename std::iterator_traits<Iter1>::difference_type size_type_1;
  typedef typename std::iterator_traits<Iter2>::difference_type size_type_2;
  typedef typename std::iterator_traits<Iter3>::difference_type size_type_3;

  size_type_1 size_1 = std::distance(first1,last1);
  size_type_2 size_2 = std::distance(first2,last2);
  size_type_3 size_3 = std::distance(first3,last3);


  std::vector<std::vector<std::vector<int> > > table(
    size_1 + 1,
    std::vector<int>(
      size_2 + 1,
      std::vector<int>(size_3 + 1, 0)
    )
  );

  Iter2 y;
  Iter3 z;

  for (int i = 1; i < size_1 + 1; ++i)
  {
    y = first2;

    for (int j = 1; j < size_2 + 1; ++j)
    {
      z = first3;

      for (int k = 1; k < size_3 + 1; ++k)
      {
        if (*first1 == *y && *y == *z)
          table[i][j][k] = table[i-1][j-1][k-1] + 1;
        else
          table[i][j][k] = std::max(
            std::max(table[i-1][j-1][k], table[i][j-1][k-1]),
            table[i-1][j][k-1]
          );
        ++z;
      }
      ++y;
    }
    ++first1;
  }

  // Haven't implemented backtracking for this one yet.

  return table[size_1][size_2][size_3];
}


int main()
{
  std::string s1, s2;
  while (std::cin >> s1 >> s2)
  {
    auto lcs = longest_common_subsequence(
      s1.begin(), s1.end(), s2.begin(), s2.end()
    );

    for (auto const& x : lcs)
      std::cout << x;
    endl(std::cout);
  }
}

#include <vector>

// I don't remember what this does... :S
struct sum_combination
{
  using lld = long long;

  std::vector<std::size_t> m;

  sum_combination()
    : m({1,1,1,2})
  {
  }

  std::size_t operator()(lld n)
  {
    if (n < 0)
      return 0;

    m.resize(n);

    for (lld i = 4; i <= n; ++i)
      m[i] = m[i-1] + m[i-3] + m[i-4];

    return m[n];
  }

}; // struct sum_combination

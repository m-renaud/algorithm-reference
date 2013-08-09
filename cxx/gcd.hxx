// Greatest Common Divisor

template <typename T>
T gcd(T m, T n)
{
  for(T rem = m % n; rem != 0; rem = m % n)
  {
    m = n;
    n = rem;
  }
  return n;
}

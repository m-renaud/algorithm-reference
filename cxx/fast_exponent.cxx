// Calculating Exponents in lg(n) time.

template <typename BaseType, typename ExponentType>
BaseType fast_exponent(BaseType a, ExponentType n)
{
  if(n == 0)
    return 1;
  if(n == 1)
    return a;

  BaseType result = fast_exponent(a, n/2);

  return result * result * fast_exponent(a, n % 2);
}

template <typename BaseType, typename ExponentType>
BaseType fast_exponent_non_rec(BaseType a, ExponentType n)
{
  BaseType return_value = 1;

  while(n != 0)
  {
    if((n % 2) == 1)
      return_value *= a;

    a *= a;
    n /= 2;
  }

  return return_value;
}


template <typename T>
T square(T const& x)
{
  return x * x;
}

template <typename T>
T exp_mod(T const& a, T const& n, T const& m)
{
  if (n == 0)
    return T{1};

  if (n == 1)
    return a % m;

  if(n % 2 == 0)
    return square(exp_mod(a, n/2, m) % m) % m;
  else
    return ((square(exp_mod(a, n/2, m) % m) % m) * (a%m) ) % m;
}


int main()
{

}

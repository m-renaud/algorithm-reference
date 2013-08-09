#include <iostream>

// Online Average Calculation.
//   From: The Art of Computer Programming Vol. 2, Third Edition, Page 232
//
//   Original:
//     M_1 = x_1;
//     M_k = M_{k-1} + (x_k - M_{k-1} / k;
//

// Implemented the algorithm presented so it is online.
//
// M : Previous mean.
// x : Next value in the sequence.
// k : Element number you are processing.
//
// Note: k does not get modified by the algorithm, you are responsible
//       for doing that.
template <typename T, typename Count>
T online_avg(T M, T const& x, Count k)
{
  return M += (x - M) / k;
}

int main()
{
  int count = 1;
  double avg = 0;

  for (double val; std::cin >> val; )
  {
    avg = online_avg(avg, val, count++);
    std::cout << "Average: " << avg << std::endl;
  }
}

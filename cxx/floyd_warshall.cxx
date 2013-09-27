#include <iostream>
#include <vector>

// Floyd-Warshall Algorithm
//
// Graph algorithm for finding all pairs shortest path
// in a weighted graph positive or negative edge weights, but no cycles.
//
// See: http://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm

// Modifies DIST to contain the the shortest path cost between nodes.
// A value of 0 means no path is available.
void floyd_warshall(std::vector<vector<int> >& dist)
{
	std::size_t n = dist.size();

	int i, j, k;

	for (k = 0; k < n; ++k)
		for (i = 0; i < n; ++i)
			for (j = 0; j < n; ++j)
				if ((dist[i][k] * dist[k][j] != 0) && (i != j))
					if ((dist[i][k] + dist[k][j] < dist[i][j]) || (dist[i][j] == 0))
						dist[i][j] = dist[i][k] + dist[k][j];
}


int main()
{
	std::vector<std::vector<int> >& distance;
}

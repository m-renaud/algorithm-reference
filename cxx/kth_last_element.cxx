#include <iostream>
#include <iterator>
#include <vector>

#include "list.cxx"

// Given a node in a linked list, find the kth last element from the end.
// If there are less than k elements in the list,
template <typename T>
slist_node<T>* kth_last_element(slist_node<T>* head, int k)
{
	slist_node<T>* runner = head;
	for (int i = 0; i < k; ++i)
	{
		if (runner == nullptr)
			return nullptr;

		runner = runner->next;
	}

	while (runner != nullptr)
	{
		head = head->next;
		runner = runner->next;
	}

	return head;
}


int main()
{
	int k;
	std::cin >> k;

	std::vector<int> vec(
		std::istream_iterator
	);

	slist_node<int>* l = build_list(vec.begin(), vec.end());

	auto result = kth_last_element(l, 20);
	if (result)
		std::cout << result->val << std::endl;
	else
		std::cout << "There are less than " << k << " items in the list.\n";
}

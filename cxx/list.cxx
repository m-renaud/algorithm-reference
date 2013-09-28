#include <iostream>
#include <utility>

// Bare implementation of a singly linked list to be used in low-level
// algorithm implementation.

template <typename T>
struct slist_node
{
	slist_node()
		: slist_node(0, nullptr)
	{
	}

	slist_node(T const& v)
		: slist_node(v, nullptr)
	{
	}

	slist_node(T const& v, slist_node<T>* const n)
		: val(v), next(n)
	{
	}

	T val;
	slist_node<T>* next;
};


template <typename Iter>
slist_node<typename Iter::value_type>* build_list(Iter begin, Iter end)
{
	using value_type = typename Iter::value_type;
	slist_node<value_type>* head = nullptr;
	slist_node<value_type>* tail = nullptr;

	while (begin != end)
	{
		slist_node<value_type>* n = new slist_node<value_type>(*begin++, nullptr);

		if (head == nullptr)
			head = n;

		if (tail != nullptr)
			tail->next = n;

		tail = n;
	}

	return head;
}

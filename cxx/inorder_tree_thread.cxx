#include <iostream>
#include <utility>

template <typename T>
struct node
{
	T datum;
	node<T>* left;
	node<T>* right;
	node<T>* next;
};

template <typename T>
std::pair<T*, T*> thread_tree(node<T>& cur)
{
	if (cur == nullptr)
		return {nullptr, nullptr};

	if (cur->left == nullptr && cur->right == nullptr)
		return {cur, cur};

	auto lhs = thread_tree(cur->left);
	auto rhs = thread_tree(cur->right);

	if (lhs.second)
		lhs.second->next = cur;

	cur->next = rhs.first;

	return {lhs.first ? lhs.first : cur, rhs.second};
}

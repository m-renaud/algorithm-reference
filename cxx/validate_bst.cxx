#include <iostream>
#include <iomanip>
#include <utility>

// Validate that a BST is in a defined state.
// ie. All left children are less than the root and all right
//     children are greater than (or equal to) the root.


//m=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
template <typename T>
struct maybe
{
	maybe()
		: set(false)
	{
	}

	maybe<T>& operator =(T const& v_)
	{
		v = v_;
		set = true;
		return *this;
	}

	operator T& ()
	{
		return v;
	}

	T v;
	bool set;
};


//m=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
template <typename T>
struct bst_node
{
	bst_node(T const& v)
		: val(v)
	{
	}

	T val;
	bst_node<T>* left;
	bst_node<T>* right;
};


//m=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
template <typename T>
struct bst
{
	bst()
		: root(nullptr)
	{
	}


	bool validate()
	{
		maybe<T> m;
		return validate_impl(m, root);
	}


	static bool validate_impl(maybe<T>& last_val, bst_node<T> const* current)
	{
		if (current == nullptr)
			return true;

		if (validate_impl(last_val, current->left) == false)
			return false;


		if (last_val.set)
			if (current->val < last_val)
				return false;

		last_val = current->val;

		return validate_impl(last_val, current->right);
	}

	bst_node<T>* root;
};

int main()
{
	bst<int> b;

	std::cout << std::boolalpha << b.validate() << std::endl;

	b.root = new bst_node<int>(10);
	std::cout << std::boolalpha << b.validate() << std::endl;

	b.root->left = new bst_node<int>(1);
	std::cout << std::boolalpha << b.validate() << std::endl;

	b.root->right = new bst_node<int>(15);
	std::cout << std::boolalpha << b.validate() << std::endl;

	b.root->left->val = 100;
	std::cout << std::boolalpha << b.validate() << std::endl;

	delete b.root->left;
	delete b.root->right;
	delete b.root;

	return 0;
}

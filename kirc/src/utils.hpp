#ifndef KIWI_UTILS_HPP
#define KIWI_UTILS_HPP

#include <vector>
#include <iostream>

#ifndef NDEBUG
#define k_assert(x) for (;!(x);assert(x))
#define k_massert(x, m) for (;!(x);assert(x)) { std::cout << __FILE__ << ":" << __LINE__ << ": " << m }
#define k_dbg(m) std::cout << __FILE__ << ":" << __LINE__ << ": " << m
#elif
#define k_assert(x)
#define k_massert(x, m)
#define k_dbg(m)
#endif

namespace KIR
{
	template <class T>
	void deleteAll(std::vector<T*>& vec)
	{
		for (typename std::vector<T*>::iterator it = vec.begin(); it < vec.end(); it++)
		{
			delete *it;
		}
		vec.clear();
	}

	template<class U, class T>
	bool isa(T* pt)
	{
		return dynamic_cast<U*>(pt) != NULL;
	}

	class Stringable
	{
	public:
		virtual std::string toString() = 0;
	};

	class Cloneable
	{
	public:
		virtual Cloneable* clone() = 0;
	};
}

#endif
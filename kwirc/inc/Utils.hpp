#ifndef KIWI_UTILS_HPP
#define KIWI_UTILS_HPP

#include <vector>
#include <cassert>
#include <algorithm>
#include <iterator>

#include "utils/Object.hpp"
#include "utils/MemoryContext.hpp"

#define k_extract(VARNAME, CLASS, EXPR) if (CLASS* VARNAME = dynamic_cast<CLASS*>(EXPR))

#ifndef KIWI_NDEBUG
#define k_assert(x) for (;!(x);assert(x))
#define k_massert(x, m) for (;!(x);assert(x)) std::cout << __FILE__ << ", line " << __LINE__ << ": " << m << std::endl
#define k_dbg(m) std::cout << __FILE__ << ", line " << __LINE__ << ": " << m << std::endl
#else
#define k_assert(x)
#define k_massert(x, m)
#define k_dbg(m)
#endif

typedef __int128_t cent;
typedef __uint128_t ucent;

template<typename T> using RP = std::shared_ptr<T>;
template <typename T, typename... Args>
auto newRP(Args&&... args) -> decltype(std::make_shared<T>(std::forward<Args>(args)...)) 
{
    return std::make_shared<T>(std::forward<Args>(args)...);
}

template<typename T> using UP = std::shared_ptr<T>;
template <typename T, typename... Args>
auto newUP(Args&&... args) -> decltype(std::make_shared<T>(std::forward<Args>(args)...)) 
{
    return std::make_shared<T>(std::forward<Args>(args)...);
}

namespace KIR
{
	template<typename T>
	typename std::enable_if<std::is_integral<T>::value, T>::type hash(std::initializer_list<T> list, T prime = 31)
	{
		T res = 1;
		for (T t : list)
		{
			res = prime * res + t;
		}
		return res;
	}

	template<typename T = size_t, typename It>
	typename std::enable_if<std::is_integral<T>::value && std::is_same<typename std::iterator_traits<It>::value_type, T>::value, T>::type hash(It begin, It end, T prime = 31)
	{
		T res = 1;
		for (auto it = begin; it < end; it++)
		{
			res = prime * res + *it;
		}
		return res;
	}

	template<typename T>
	class ExtensibleList : private std::vector<T>
	{
		typedef std::vector<T> vector;
	protected:
		inline ExtensibleList(std::initializer_list<T> _init) : vector(_init) {}
		template<typename It>
		inline ExtensibleList(It begin, It end) : vector(begin, end) {}
		inline ExtensibleList(vector _init) : vector(_init) {}
		inline ExtensibleList() : vector() {}
	public:
		using vector::push_back;
		using vector::insert;
		using vector::operator[];
		using vector::begin;
		using vector::end;
		using vector::cbegin;
		using vector::cend;
		using vector::size;
		using vector::empty;
		using vector::erase;
	};
}

#endif
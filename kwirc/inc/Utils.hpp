#ifndef KIWI_UTILS_HPP
#define KIWI_UTILS_HPP

#include <vector>

#include "utils/Object.hpp"
#include "utils/MemoryContext.hpp"

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
	class ExtensibleList : private std::vector<T>
	{
		typedef std::vector<T> vector;
	protected:
		inline ExtensibleList(std::initializer_list<T> _init) : vector(_init) {}
		inline ExtensibleList(vector _init) : vector(_init) {}
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
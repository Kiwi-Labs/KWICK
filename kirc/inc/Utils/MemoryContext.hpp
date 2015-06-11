#ifndef KIWI_MEMCTX_HPP
#define KIWI_MEMCTX_HPP

#include <iostream>

#include <typeinfo>
#include <memory>
#include <unordered_set>
#include <set>
#include <vector>
#include <unordered_map>

namespace KIR
{
	template<typename T>
	struct MemoryContext
	{
	private:
		std::unordered_set<T*> t_ptrs;
	public:
		T* add(T* t_ptr)
		{
			t_ptrs.insert(t_ptr);
			return t_ptr;
		}
		T* remove(T* t_ptr)
		{
			t_ptrs.erase(t_ptr);
			return t_ptr;
		}
		void destroy()
		{
			for (T* t_ptr : t_ptrs)
			{
				delete t_ptr;
			}
		}
		~MemoryContext()
		{
			destroy();
		}
	};
}

#endif
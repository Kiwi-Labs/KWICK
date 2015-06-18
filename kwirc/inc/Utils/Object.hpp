#ifndef KIWI_OBJECT_HPP
#define KIWI_OBJECT_HPP

#include <iostream>

namespace KIR
{
	template<typename T, typename U> struct MemoryContext;

	class Object
	{
	protected:
		virtual ~Object() {}
		template<typename T, typename U>
		friend struct MemoryContext;
	};
}

#endif
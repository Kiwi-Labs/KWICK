#ifndef KIWI_OBJECT_HPP
#define KIWI_OBJECT_HPP

#include <iostream>

#include "Utils/MemoryContext.hpp"

namespace KIR
{
	class KIRObject
	{
	protected:
		virtual ~KIRObject() {}
		template<typename T>
		friend struct MemoryContext<T>;
	};
}

#endif
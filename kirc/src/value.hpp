#ifndef KIWI_VALUE_HPP
#define KIWI_VALUE_HPP

#include "type.hpp"

namespace KIR
{
	class Value
	{
	public:
		virtual Type* getType() = 0;
	};
}

#endif
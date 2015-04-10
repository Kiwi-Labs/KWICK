#include "type.hpp"

#ifndef KIWI_VALUE_HPP
#define KIWI_VALUE_HPP

namespace KIR
{
	class Value
	{
		virtual Type getType() = 0;
	};
}

#endif
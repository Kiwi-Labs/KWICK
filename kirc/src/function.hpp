#include "value.hpp"

#ifndef KIWI_FUNCTION_HPP
#define KIWI_FUNCTION_HPP

namespace KIR
{
	// ---------------------------------------------------------------------------------------------------------------- //
	// KIR function. This is one of the fundamental building blocks of Kiwi IR, since this is the only actual container
	// for instructions available for use in a KIRModule.

	class Function: Value
	{
	public:
		std::string name;
	};
}

#endif
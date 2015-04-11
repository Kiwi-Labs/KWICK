#ifndef KIWI_FUNCTION_HPP
#define KIWI_FUNCTION_HPP

#include "value.hpp"
#include "type.hpp"
#include "symbol.hpp"

namespace KIR
{
	// ---------------------------------------------------------------------------------------------------------------- //
	// KIR function. This is one of the fundamental building blocks of Kiwi IR, since this is the only actual container
	// for instructions available for use in a KIRModule.

	class Function: public Value, public Symbol
	{
		FunctionType* ftype;
	public:
		Type* getType() { return ftype; }
	};
}

#endif
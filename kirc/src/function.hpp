#ifndef KIWI_FUNCTION_HPP
#define KIWI_FUNCTION_HPP

#include <sstream>
#include <vector>
#include <algorithm>

#include "value.hpp"
#include "type.hpp"
#include "symbol.hpp"
#include "module.hpp"

namespace KIR
{
	// ---------------------------------------------------------------------------------------------------------------- //
	// KIR function. This is one of the fundamental building blocks of Kiwi IR, since this is the only actual container
	// for instructions available for use in a KIRModule.

	class Binding;
	class Function: public Value, public Symbol, public Definition
	{
		FunctionType* ftype;
		Block* block;
		SymbolIdentifier id;

		std::vector<Binding*> args;
		std::vector<Binding*> rets;
	public:
		Function(std::vector<Binding*> _args, std::vector<Binding*> _rets);
		~Function()
		{
			delete ftype;
		}
		FunctionType* getType() { return ftype; }
		SymbolIdentifier getIdentifier() { return id; }
		std::string toString();
	};
}

#endif
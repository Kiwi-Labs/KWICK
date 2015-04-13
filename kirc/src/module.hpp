#ifndef KIWI_MODULE_HPP
#define KIWI_MODULE_HPP

#include <string>
#include <vector>

#include "symbol.hpp"

namespace KIR
{
	class Definition: public Symbol, public Stringable
	{

	};

	// ---------------------------------------------------------------------------------------------------------------- //
	// KIR module, representing a file.

	class Function;
	class Module
	{
		std::string name;
		std::vector<Definition*> defs;
		SymbolTable* symtab;
	public:
		Module(std::string name);
		void addDefinition(SymbolIdentifier id, Definition* def)
		{
			symtab->setSymbol(id, def);
			defs.push_back(def);
		}
	};
}

#endif
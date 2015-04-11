#ifndef KIWI_SYMBOL_CPP
#define KIWI_SYMBOL_CPP

#include "symbol.hpp"

namespace KIR
{
	void SymbolTable::setSymbol(SymbolIdentifier id, Symbol* sym)
	{
		if (!map.count(id)) map[id] = sym;
		else map[id] = new SymError((std::stringstream("Symbol with id ") << id << " is already defined in block " << parent->getName() << ".").str());
	}
}

#endif
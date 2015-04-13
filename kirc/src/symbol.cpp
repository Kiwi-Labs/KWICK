#ifndef KIWI_SYMBOL_CPP
#define KIWI_SYMBOL_CPP

#include "symbol.hpp"
#include <iostream>

namespace KIR
{
	bool SymbolTable::containsSymbol(SymbolIdentifier id)
	{
		if (map.count(id) == 0)
		{
			for (SymbolTable* parent : parents)
			{
				if (parent->containsSymbol(id)) return true;
			}
			return false;
		}
		return true;
	}
	Symbol* SymbolTable::getSymbol(SymbolIdentifier id)
	{
		if (!containsSymbol(id)) return NULL;
		else if (map.count(id) > 0) return map[id];
		else
		{
			for (SymbolTable* parent : parents)
			{
				if (Symbol* sym = parent->getSymbol(id)) return sym;
			}
		}
		return NULL;
	}
	bool SymbolTable::setSymbol(SymbolIdentifier id, Symbol* sym)
	{
		if (!SymbolTable::containsSymbol(id))
		{
			map[id] = sym;
			return true;
		}
		else
		{
			setPredefinedSymbol(id, new SymError((std::stringstream("Symbol with id ") << id << " is already defined!").str()));
			return false;
		}
	}
	void SymbolTable::setPredefinedSymbol(SymbolIdentifier id, Symbol* sym)
	{
		if (map.count(id) > 0)
		{
			map[id] = sym;
		}
		else if (parents.size() > 0)
		{
			for (SymbolTable* parent : parents) if (parent->containsSymbol(id)) parent->setPredefinedSymbol(id, sym);
		}
		else
		{
			setSymbol(id, new SymError((std::stringstream("Symbol with id ") << id << " is NOT already defined!").str()));
		}
	}
	std::string SymbolTable::toString()
	{
		return "$SYMBOLTABLE_STUB"; // Don't you fucking dare to say anything about this
	}
}

#endif
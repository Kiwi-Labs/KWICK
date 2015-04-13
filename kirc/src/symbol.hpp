#ifndef KIWI_SYMBOL_HPP
#define KIWI_SYMBOL_HPP

#include <unordered_map>
#include <string>
#include <sstream>

#include "value.hpp"
#include "type.hpp"
#include "utils.hpp"

namespace KIR
{
	typedef std::string SymbolIdentifier;

	class Symbol
	{

	};

	class SymError: public Symbol
	{
	public:
		std::string errordesc;
		SymError(std::string _errordesc) : errordesc(_errordesc) {}
	};

	class Block;

	class SymbolTable : public Stringable
	{
		std::unordered_map<SymbolIdentifier, Symbol*> map;
		std::vector<SymbolTable*> parents;
		void setPredefinedSymbol(SymbolIdentifier id, Symbol* sym);
	public:
		SymbolTable() {}
		SymbolTable(SymbolTable* _parent) : parents({ _parent }) {}
		~SymbolTable()
		{
			map.clear();
		}
		bool containsSymbol(SymbolIdentifier id);
		Symbol* getSymbol(SymbolIdentifier id);
		bool setSymbol(SymbolIdentifier id, Symbol* sym);
		std::string toString();
	};
}

#endif
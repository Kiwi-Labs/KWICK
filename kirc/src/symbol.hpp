#ifndef KIWI_SYMBOL_HPP
#define KIWI_SYMBOL_HPP

#include <unordered_map>
#include <string>
#include <sstream>

#include "value.hpp"
#include "type.hpp"

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

	class SymbolTable
	{
		std::unordered_map<SymbolIdentifier, Symbol*> map;
		Block* parent;
	public:
		SymbolTable(Block* _parent) : parent(_parent) {}
		~SymbolTable()
		{
			map.clear();
		}
		bool containsSymbol(SymbolIdentifier id) { return map.count(id); }
		Symbol* getSymbol(SymbolIdentifier id) { return map[id]; }
		void setSymbol(SymbolIdentifier id, Symbol* sym);
	};

	class Binding: public Value, public Symbol
	{
		Type* type;
		const std::string& ident;
		const size_t& loc; // To prevent forward reference of bindings.
		const bool mut; // Mutable?
		Binding(Type* _type, const std::string& _ident, const size_t& _loc, const bool _mut) : type(_type), ident(_ident), loc(_loc), mut(_mut) {}
	public:
		Type* getType()
		{
			return this->type;
		}
		const std::string getIdentifier()
		{
			return this->ident;
		}
		const size_t getBlockRelativeLocation()
		{
			return this->loc;
		}
	};
}

#endif
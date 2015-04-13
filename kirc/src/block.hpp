#ifndef KIWI_BLOCK_HPP
#define KIWI_BLOCK_HPP

#include <iostream>

#include "statement.hpp"
#include "symbol.hpp"

namespace KIR
{
	class Block: public Statement
	{
		Block* parent;
		std::string name;
		SymbolTable* symtab;
		Block() {}
	public:
		std::vector<Statement*> statements;
		Block(Block* _parent, std::string _name = "") : parent(_parent), name(_name), symtab(new SymbolTable(_parent->symtab)) {}
		Block(std::string _name) : name(_name), parent(NULL), symtab(new SymbolTable()) {}
		~Block()
		{
			deleteAll(statements);
			delete symtab;
		}
		std::string getName() { return name; }
		void pushStatement(Statement* statement);
		void insertStatement(Statement* statement, size_t loc);
		bool validate()
		{
			for (auto it = statements.begin(); it < statements.end(); it++)
			{
				if (!(*it)->validate()) return false;
			}
			return true;
		}
		Block* clone()
		{
			Block* res = new Block();
			res->parent = this->parent;
			res->name = this->name;
			res->symtab = this->symtab;
			return res;
		}
		std::string toString()
		{
			std::stringstream out;
			for (auto it = this->statements.begin(); it < this->statements.end(); it++)
			{
				out << (*it)->toString() << std::endl;
			}
			return out.str();
		}
	};
}

#endif
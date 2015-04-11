#ifndef KIWI_BLOCK_HPP
#define KIWI_BLOCK_HPP

#include "symbol.hpp"
#include "statement.hpp"

namespace KIR
{
	class Block: public Statement
	{
		std::string name;
		SymbolTable& symtab;

		std::vector<Statement*> statements;
	public:
		Block(std::string _name) : name(_name)
		{
			symtab = new SymbolTable(name);
		}
		~Block()
		{
			statements.clear();
		}
		std::string getName() { return name; }
		void pushStatement(Statement* statement)
		{
			statements.push_back(statement);
		}
		void insertStatement(Statement* statement, const size_t loc)
		{
			statements.insert(statements.begin()+loc, statement);
			for (std::vector<Statement*>::iterator it = statements.begin()+loc+1; it < statements.end(); it++)
			{
				(*it)->loc++;
			}
		}
	};
}

#endif
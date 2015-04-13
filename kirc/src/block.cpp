#ifndef KIWI_BLOCK_CPP
#define KIWI_BLOCK_CPP

#include <iostream>

#include "block.hpp"
#include "statement.hpp"
#include "utils.hpp"

namespace KIR
{
	void Block::pushStatement(Statement* statement)
	{
		statement->parent = this;
		statement->loc = statements.size();
		if (Binding* binding = dynamic_cast<Binding*>(statement))
		{
			if (symtab->setSymbol(binding->getIdentifier(), binding))
			{
				statements.push_back(statement);
			}
			return;
		}
		statements.push_back(statement);
	}
	void Block::insertStatement(Statement* statement, const size_t loc)
	{
		statement->parent = this;
		statement->loc = loc;
		if (Binding* binding = dynamic_cast<Binding*>(statement)) if (!symtab->setSymbol(binding->getIdentifier(), binding)) return;
		statements.insert(statements.begin()+loc, statement);
		for (std::vector<Statement*>::iterator it = statements.begin()+(loc+1); it < statements.end(); it++)
		{
			(*it)->loc++;
		}
	}
}

#endif
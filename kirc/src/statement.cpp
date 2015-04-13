#ifndef KIWI_STATEMENT_CPP
#define KIWI_STATEMENT_CPP

#include <iostream>

#include "statement.hpp"
#include "block.hpp"
#include "utils.hpp"

namespace KIR
{
	IfBranch::~IfBranch()
	{
		delete cond;
		delete true_block;
		delete false_block;
	}
	IfBranch* IfBranch::clone()
	{
		return new IfBranch(cond->clone(), true_block->clone(), (false_block != NULL ? false_block->clone() : NULL));
	}
	bool IfBranch::validate()
	{
		return isa<IntegerType>(cond->getType()) && true_block != NULL; // True block can't be null, but else block can.
	}
	std::string IfBranch::toString()
	{
		std::stringstream out;
		out << "if (" << cond->toString() << ")" << std::endl << "  " << true_block->toString();
		if (false_block != NULL) out << "else" << std::endl << "  " << false_block->toString();
		return out.str();
	}
}

#endif
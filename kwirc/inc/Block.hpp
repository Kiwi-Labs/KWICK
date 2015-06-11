#ifndef KIWI_BLOCK_HPP
#define KIWI_BLOCK_HPP

#include <vector>

#include "Utils.hpp"
#include "Statement.hpp"
#include "Expression.hpp"

namespace KIR
{
	class Block : public Expression, public ExtensibleList<Statement*>
	{
		
	};
}

#endif
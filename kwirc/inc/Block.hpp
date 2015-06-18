#ifndef KIWI_BLOCK_HPP
#define KIWI_BLOCK_HPP

#include <vector>

#include "Utils.hpp"
#include "Type.hpp"
#include "Statement.hpp"
#include "Expression.hpp"

namespace KIR
{
	class Block : public Expression, public ExtensibleList<Statement*>
	{
		Type* kir_t; // 'value' statement return.
	public:
		Block() : kir_t(Type::getVoidType()) {}
		Block(Type* _kir_t) : kir_t(_kir_t) {}
	};
}

#endif
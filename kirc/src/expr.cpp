#ifndef KIWI_EXPR_CPP
#define KIWI_EXPR_CPP

#include "objdef.hpp"
#include "expr.hpp"
#include "type.hpp"

namespace KIR
{
	Accessible* AccessExpression::getLHSDefinition()
	{
		if (ClassType* ct = dynamic_cast<ClassType*>(lhs->getType()))
		{
			return ct->class_def;
		}
		else if (StructType* st = dynamic_cast<StructType*>(lhs->getType()))
		{
			return st->struct_def;
		}
		else
		{
			return NULL;
		}
	}
	Type* AccessExpression::getType()
	{
		if (Accessible* acc = getLHSDefinition()) return acc->getBindingType(id);
		else return NULL;
	}
	bool AccessExpression::validate()
	{
		return getLHSDefinition()->containsBinding(id);
	}

	Type* LoadExpression::getType()
	{
		return bd->getType();
	}
	std::string LoadExpression::toString()
	{
		return bd->getIdentifier();
	}
}

#endif
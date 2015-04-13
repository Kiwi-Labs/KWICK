#ifndef KIWI_TYPE_CPP
#define KIWI_TYPE_CPP

#include "type.hpp"
#include "objdef.hpp"

namespace KIR
{
	std::string StructType::toString()
	{
		return struct_def->toString();
	}

	bool StructType::isAssignableTo(Type& type)
	{
		if (StructType* st = dynamic_cast<StructType*>(&type))
		{
			return this->struct_def == st->struct_def || this->struct_def->inheritsFrom(st->struct_def);
		}
		return false;
	}

	std::string ClassType::toString()
	{
		return class_def->toString();
	}

	bool ClassType::isAssignableTo(Type& type)
	{
		if (ClassType* ct = dynamic_cast<ClassType*>(&type))
		{
			return this->class_def == ct->class_def || this->class_def->inheritsFrom(ct->class_def);
		}
		return false;
	}
}

#endif
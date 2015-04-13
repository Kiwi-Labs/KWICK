#ifndef KIWI_FUNCTION_CPP
#define KIWI_FUNCTION_CPP

#include "function.hpp"
#include "statement.hpp"

namespace KIR
{
	Function::Function(std::vector<Binding*> _args, std::vector<Binding*> _rets) : args(_args), rets(_rets)
	{
		std::vector<Type*> atvec;
		std::transform(args.begin(), args.end(), std::back_inserter(atvec), [](Binding* b) { return b->getType(); });
		TupleType* atype = new TupleType(atvec);

		std::vector<Type*> rtvec;
		std::transform(rets.begin(), rets.end(), std::back_inserter(rtvec), [](Binding* b) { return b->getType(); });
		TupleType* rtype = new TupleType(rtvec);

		ftype = new FunctionType(atype, rtype);
	}

	std::string Function::toString()
	{
		std::stringstream out;
		out << "func " << id << "(";
		if (args.size() > 0) out << args[0]->toString();
		for (auto it = args.begin()+1; it < args.end(); it++)
		{
			out << ", " << (*it)->toString();
		}
		out << ") -> (";
		if (rets.size() > 0) out << rets[0]->toString();
		for (auto it = rets.begin()+1; it < rets.end(); it++)
		{
			out << ", " << (*it)->toString();
		}
		out << ")" << std::endl;
		out << block->toString();
		return out.str();
	}
}

#endif
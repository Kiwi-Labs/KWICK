#ifndef KIWI_CONTEXT_HPP
#define KIWI_CONTEXT_HPP

#include <cassert>
#include <iostream>

#include "llvm/IR/Module.h"

#include "Utils.hpp"
#include "Type.hpp"
#include "Expression.hpp"
#include "Statement.hpp"
#include "Function.hpp"
#include "Identifier.hpp"

namespace KIR
{
	struct Context
	{
		Context();
		~Context();
		MemoryContext<Type> t_mctx;
		MemoryContext<Expression> e_mctx;
		MemoryContext<Statement> s_mctx;
		MemoryContext<Function> f_mctx;
		MemoryContext<Object> obj_mctx;
		std::unordered_map<Identifier, Function*> named_funcs;
	private:
		static void pushContext(Context& _kir_cctx);
	};

	Context& getContext();
	bool isContextValid();
}

#endif
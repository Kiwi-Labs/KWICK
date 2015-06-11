#ifndef KIWI_CONTEXT_HPP
#define KIWI_CONTEXT_HPP

#include <cassert>
#include <iostream>

#include "llvm/IR/Module.h"

#include "Utils.hpp"
#include "Type.hpp"
#include "Expression.hpp"
#include "Statement.hpp"

namespace KIR
{
	struct KIRContext
	{
		MemoryContext<Type> t_mctx;
		MemoryContext<Expression> e_mctx;
		MemoryContext<Statement> s_mctx;
		~KIRContext();
	};

	KIRContext& getContext();
	void setContext(KIRContext& _kir_cctx);
	bool isContextValid();
}

#endif
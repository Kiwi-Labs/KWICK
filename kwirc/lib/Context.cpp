#ifndef KIWI_CONTEXT_CPP
#define KIWI_CONTEXT_CPP

#include <stack>

#include "Context.hpp"

namespace KIR
{
	thread_local std::stack<Context*> kir_cctx;

	Context::Context() { kir_cctx.push(this); }
	Context::~Context() { kir_cctx.pop(); }

	Context& getContext()
	{
		std::cout << "Is context valid? " << (isContextValid() ? "true" : "false") << std::endl;
		assert(isContextValid());
		return *kir_cctx.top();
	}

	bool isContextValid()
	{
		return kir_cctx.top() != nullptr;
	}
}

#endif
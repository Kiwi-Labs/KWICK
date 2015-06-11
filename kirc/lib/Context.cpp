#ifndef KIWI_CONTEXT_CPP
#define KIWI_CONTEXT_CPP

#include "Context.hpp"

namespace KIR
{
	__thread bool kir_cvalid = false;
	__thread KIRContext* kir_cctx = nullptr;

	KIRContext::~KIRContext()
	{
		if (kir_cctx == this) kir_cvalid = false;
	}

	KIRContext& getContext()
	{
		std::cout << "Is context valid? " << (kir_cvalid ? "true" : "false") << std::endl;
		assert(kir_cvalid);
		return *kir_cctx;
	}

	void setContext(KIRContext& _kir_cctx)
	{
		kir_cctx = &_kir_cctx;
		kir_cvalid = true;
	}

	bool isContextValid()
	{
		return kir_cvalid;
	}
}

#endif
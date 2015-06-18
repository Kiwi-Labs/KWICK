#ifndef KIWI_FUNCTION_CPP
#define KIWI_FUNCTION_CPP

#include "Context.hpp"
#include "Function.hpp"

namespace KIR
{
	FunctionBody::FunctionBody()
	{
		getContext().obj_mctx.add(this);
	}

	Function::Function(Type* _kir_arg_t, Type* _kir_ret_t) : kir_arg_t(_kir_arg_t), kir_ret_t(_kir_ret_t) {}
	Function::Function(Identifier _kir_funcid, Type* _kir_arg_t, Type* _kir_ret_t) : kir_arg_t(_kir_arg_t), kir_ret_t(_kir_ret_t), kir_funcid(_kir_funcid), has_id(true)
	{
		getContext().named_funcs[kir_funcid] = this;
	}
	Function* Function::create(Type* _kir_arg_t, Type* _kir_ret_t) { return getContext().f_mctx.add(new Function(_kir_arg_t, _kir_ret_t)); }
	Function* Function::create(Type* _kir_arg_t, Type* _kir_ret_t, FunctionBody* _kir_fb)
	{
		Function* kir_func = getContext().f_mctx.add(new Function(_kir_arg_t, _kir_ret_t));
		kir_func->setBody(_kir_fb);
		return kir_func; 
	}
}

#endif
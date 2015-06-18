#ifndef KIWI_FUNCTION_HPP
#define KIWI_FUNCTION_HPP

#include "Utils.hpp"
#include "Type.hpp"
#include "Block.hpp"
#include "Identifier.hpp"

#include "llvm/IR/Function.h"

namespace KIR
{
	enum InlineMode
	{
		Always, Hint, Never
	};

	struct FunctionAttributes
	{
		InlineMode inline_mode;
	};

	class FunctionBody : public ExtensibleList<Statement*>, public Object
	{
	public:
		FunctionBody();
	};
	
	class Function : public Object
	{
		Type* kir_arg_t = nullptr;
		Type* kir_ret_t = nullptr;
		FunctionBody* kir_body = nullptr;
		Identifier kir_funcid;
		bool has_id = false;
		Function(Type*, Type*);
		Function(Identifier, Type*, Type*);
	public:
		FunctionAttributes kir_func_attribs;
		inline void setBody(FunctionBody* _kir_body)
		{
			k_massert(kir_body == nullptr, "Oh god no pls no: Function body should never be set multiple times!");
			kir_body = _kir_body;
		}
		inline bool hasBody() { return kir_body != nullptr; }
		inline FunctionBody* getBody() { return kir_body; }
		inline Type* getArgTy() { return kir_arg_t; }
		inline Type* getRetTy() { return kir_ret_t; }

		static Function* create(Type*, Type*);
		static Function* create(Type*, Type*, FunctionBody*);
	};
}

#endif
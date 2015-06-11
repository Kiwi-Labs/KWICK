#include <iostream>

#include "inc/Block.hpp"
#include "inc/Expression.hpp"
#include "inc/Function.hpp"
#include "inc/Statement.hpp"
#include "inc/Type.hpp"
#include "inc/Context.hpp"

#include "inc/Utils.hpp"

int main(int argc, char** argv)
{
	{
		KIR::KIRContext k_ctx;
		KIR::setContext(k_ctx);
		
		KIR::Type* kir_t = KIR::Type::getVoidType();
	}
}
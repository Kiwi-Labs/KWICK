#ifndef KIWI_EXPRESSION_CPP
#define KIWI_EXPRESSION_CPP

#include "Context.hpp"
#include "Expression.hpp"
#include "Utils.hpp"

#define E_MANAGED(x) getContext().e_mctx.add( (x) )

namespace KIR
{
	Expression* Expression::getBit(bool data) { return E_MANAGED(new BitExpression(data)); }

	Expression* Expression::getI8(char data) { return E_MANAGED(new I8Expression(data)); }
	Expression* Expression::getI16(short data) { return E_MANAGED(new I16Expression(data)); }
	Expression* Expression::getI32(int data) { return E_MANAGED(new I32Expression(data)); }
	Expression* Expression::getI64(long data) { return E_MANAGED(new I64Expression(data)); }
	Expression* Expression::getI128(cent data) { return E_MANAGED(new I128Expression(data)); }

	Expression* Expression::getU8(unsigned char data) { return E_MANAGED(new U8Expression(data)); }
	Expression* Expression::getU16(unsigned short data) { return E_MANAGED(new U16Expression(data)); }
	Expression* Expression::getU32(unsigned int data) { return E_MANAGED(new U32Expression(data)); }
	Expression* Expression::getU64(unsigned long data) { return E_MANAGED(new U64Expression(data)); }
	Expression* Expression::getU128(ucent data) { return E_MANAGED(new U128Expression(data)); }

	Expression* Expression::getFP32(float data) { return E_MANAGED(new FP32Expression(data)); }
	Expression* Expression::getFP64(double data) { return E_MANAGED(new FP64Expression(data)); }
	Expression* Expression::getFP80(long double data) { return E_MANAGED(new FP80Expression(data)); }
}

#endif
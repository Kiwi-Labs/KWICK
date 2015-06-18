#ifndef KIWI_EXPRESSION_CPP
#define KIWI_EXPRESSION_CPP

#include "Context.hpp"
#include "Expression.hpp"
#include "Utils.hpp"

#define E_MANAGED(x) getContext().e_mctx.add( (x) )

namespace KIR
{
	BitConstant* Expression::getBit(bool data) { return static_cast<BitConstant*>(E_MANAGED(new BitConstant(data))); }

	I8Constant* Expression::getI8(char data) { return static_cast<I8Constant*>(E_MANAGED(new I8Constant(data))); }
	I16Constant* Expression::getI16(short data) { return static_cast<I16Constant*>(E_MANAGED(new I16Constant(data))); }
	I32Constant* Expression::getI32(int data) { return static_cast<I32Constant*>(E_MANAGED(new I32Constant(data))); }
	I64Constant* Expression::getI64(long data) { return static_cast<I64Constant*>(E_MANAGED(new I64Constant(data))); }
	I128Constant* Expression::getI128(cent data) { return static_cast<I128Constant*>(E_MANAGED(new I128Constant(data))); }

	U8Constant* Expression::getU8(unsigned char data) { return static_cast<U8Constant*>(E_MANAGED(new U8Constant(data))); }
	U16Constant* Expression::getU16(unsigned short data) { return static_cast<U16Constant*>(E_MANAGED(new U16Constant(data))); }
	U32Constant* Expression::getU32(unsigned int data) { return static_cast<U32Constant*>(E_MANAGED(new U32Constant(data))); }
	U64Constant* Expression::getU64(unsigned long data) { return static_cast<U64Constant*>(E_MANAGED(new U64Constant(data))); }
	U128Constant* Expression::getU128(ucent data) { return static_cast<U128Constant*>(E_MANAGED(new U128Constant(data))); }

	FP32Constant* Expression::getFP32(float data) { return static_cast<FP32Constant*>(E_MANAGED(new FP32Constant(data))); }
	FP64Constant* Expression::getFP64(double data) { return static_cast<FP64Constant*>(E_MANAGED(new FP64Constant(data))); }
	FP80Constant* Expression::getFP80(long double data) { return static_cast<FP80Constant*>(E_MANAGED(new FP80Constant(data))); }
}

#endif
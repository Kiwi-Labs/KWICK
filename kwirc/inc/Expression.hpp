#ifndef KIWI_EXPRESSION_HPP
#define KIWI_EXPRESSION_HPP

#include "Statement.hpp"
#include "Type.hpp"
#include "Utils.hpp"

#define DEFNUMLITEXPR(N, T)											\
	class N ## Constant : public IntegerConstant<T>					\
	{ 																\
		N ## Constant(T t) : IntegerConstant<T>(t) {}				\
		Type* getType() { return Type::get ## N ## Type(); }		\
		friend class Expression;									\
	} // This comment exists for no other reason than getting rid of that fucking annoying "ERROR ERROR ERROR" syntax highlighting please																 

namespace KIR
{
	class Function;

	class BitConstant;

	class I8Constant;
	class I16Constant;
	class I32Constant;
	class I64Constant;
	class I128Constant;

	class U8Constant;
	class U16Constant;
	class U32Constant;
	class U64Constant;
	class U128Constant;

	class FP32Constant;
	class FP64Constant;
	class FP80Constant;

	class CallExpression;

	class Expression : public Statement
	{
	protected:
		Expression() {}
	public:
		virtual Type* getType() = 0;

		static BitConstant* getBit(bool);

		static I8Constant* getI8(char);
		static I16Constant* getI16(short);
		static I32Constant* getI32(int);
		static I64Constant* getI64(long);
		static I128Constant* getI128(cent);

		static U8Constant* getU8(unsigned char);
		static U16Constant* getU16(unsigned short);
		static U32Constant* getU32(unsigned int);
		static U64Constant* getU64(unsigned long);
		static U128Constant* getU128(ucent);

		static FP32Constant* getFP32(float);
		static FP64Constant* getFP64(double);
		static FP80Constant* getFP80(long double);

		static CallExpression* createCall(Function*, Expression*);	
	};

	template<typename T>
	class Constant : public Expression
	{
	protected:
		Constant() {}
	public:
		virtual T getValue() = 0;
	};

	template<typename T>
	class IntegerConstant : public Constant<T>
	{
		T val_t;
	protected:
		IntegerConstant(T _val_t) : val_t(_val_t) {}
	public:
		T getValue() { return val_t; }
	};

	DEFNUMLITEXPR(Bit, bool);

	DEFNUMLITEXPR(I8, char);
	DEFNUMLITEXPR(I16, short);
	DEFNUMLITEXPR(I32, int);
	DEFNUMLITEXPR(I64, long);
	DEFNUMLITEXPR(I128, cent);

	DEFNUMLITEXPR(U8, unsigned char);
	DEFNUMLITEXPR(U16, unsigned short);
	DEFNUMLITEXPR(U32, unsigned int);
	DEFNUMLITEXPR(U64, unsigned long);
	DEFNUMLITEXPR(U128, ucent);

	DEFNUMLITEXPR(FP32, float);
	DEFNUMLITEXPR(FP64, double);
	DEFNUMLITEXPR(FP80, long double);

	class CallExpression
	{
		Object* kir_op;
		Expression* kir_args;
		friend class Expression;
	};
}

#endif
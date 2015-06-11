#ifndef KIWI_EXPRESSION_HPP
#define KIWI_EXPRESSION_HPP

#include "Utils.hpp"
#include "Type.hpp"
#include "Statement.hpp"

#define DEFNUMLITEXPR(N, T)											\
	class N ## Expression : public IntegerConstantExpression<T> 		\
	{ 																\
		N ## Expression(T t) : IntegerConstantExpression<T>(t) {}	\
		Type* getType() { return Type::get ## N ## Type(); }		\
		friend class Expression;									\
	} // This comment exists for no other reason than getting rid of that fucking annoying "ERROR ERROR ERROR" syntax highlighting please																 

namespace KIR
{
	class Expression : public Statement
	{
	protected:
		Expression() {}
	public:
		virtual Type* getType() = 0;

		static Expression* getBit(bool);

		static Expression* getI8(char);
		static Expression* getI16(short);
		static Expression* getI32(int);
		static Expression* getI64(long);
		static Expression* getI128(cent);

		static Expression* getU8(unsigned char);
		static Expression* getU16(unsigned short);
		static Expression* getU32(unsigned int);
		static Expression* getU64(unsigned long);
		static Expression* getU128(ucent);

		static Expression* getFP32(float);
		static Expression* getFP64(double);
		static Expression* getFP80(long double);		
	};

	template<typename T>
	class ConstantExpression : public Expression
	{
	protected:
		ConstantExpression() {}
	public:
		virtual T getValue() = 0;
	};

	template<typename T>
	class IntegerConstantExpression : public ConstantExpression<T>
	{
		T val_t;
	protected:
		IntegerConstantExpression(T _val_t) : val_t(_val_t) {}
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
}

#endif
#ifndef KIWI_TYPE_HPP
#define KIWI_TYPE_HPP

#include "Utils.hpp"

namespace KIR
{
	class Type;
	class PointerType;
	class ArrayType;

	enum Kind
	{
		Void,
		Pointer,
		Bit,
		U8, U16, U32, U64, U128,
		I8, I16, I32, I64, I128,
		FP32, FP64, FP80,
		Array,
		Tuple
	};

	class Type;
	bool operator==(const Type&, const Type&);

	class Type : public KIRObject
	{
		Kind kir_kind;
	protected:
		Type(Kind);
	public:
		static Type* getVoidType();
		static Type* getPtrType(Type*);
		static Type* getBitType();
		static Type* getI8Type();
		static Type* getI16Type();
		static Type* getI32Type();
		static Type* getI64Type();
		static Type* getI128Type();
		static Type* getU8Type();
		static Type* getU16Type();
		static Type* getU32Type();
		static Type* getU64Type();
		static Type* getU128Type();
		static Type* getFP32Type();
		static Type* getFP64Type();
		static Type* getFP80Type();
		static Type* getArrayType(Type*, size_t);
		static Type* getTupleType(std::initializer_list<Type*>);
		static Type* getTupleType(std::vector<Type*>);

		friend bool operator==(const Type&, const Type&);

		inline bool isInteger()
		{
			switch(kir_kind)
			{
				case Kind::U8:
				case Kind::U16:
				case Kind::U32:
				case Kind::U64:
				case Kind::U128:
				case Kind::I8:
				case Kind::I16:
				case Kind::I32:
				case Kind::I64:
				case Kind::I128:
				return true;
				default:
				return false;
			}
		}
		inline bool isSignedInteger()
		{
			switch(kir_kind)
			{
				case Kind::I8:
				case Kind::I16:
				case Kind::I32:
				case Kind::I64:
				case Kind::I128:
				return true;
				default:
				return false;
			}
		}
		inline bool isUnsignedInteger()
		{
			switch(kir_kind)
			{
				case Kind::U8:
				case Kind::U16:
				case Kind::U32:
				case Kind::U64:
				case Kind::U128:
				return true;
				default:
				return false;
			}
		}
		inline bool isFloat()
		{
			switch(kir_kind)
			{
				case Kind::FP32:
				case Kind::FP64:
				case Kind::FP80:
				return true;
				default:
				return false;
			}
		}


		inline bool isVoid() { return kir_kind == Kind::Void; }
		inline bool isPointer() { return kir_kind == Kind::Pointer; }
		inline bool isBit() { return kir_kind == Kind::Bit; }
		inline bool isI8() { return kir_kind == Kind::I8; }
		inline bool isI16() { return kir_kind == Kind::I16; }
		inline bool isI32() { return kir_kind == Kind::I32; }
		inline bool isI64() { return kir_kind == Kind::I64; }
		inline bool isI128() { return kir_kind == Kind::I128; }
		inline bool isU8() { return kir_kind == Kind::U8; }
		inline bool isU16() { return kir_kind == Kind::U16; }
		inline bool isU32() { return kir_kind == Kind::U32; }
		inline bool isU64() { return kir_kind == Kind::U64; }
		inline bool isU128() { return kir_kind == Kind::U128; }
		inline bool isFP32() { return kir_kind == Kind::FP32; }
		inline bool isFP64() { return kir_kind == Kind::FP64; }
		inline bool isFP80() { return kir_kind == Kind::FP80; }
		inline bool isArray() { return kir_kind == Kind::Array; }
		inline bool isTuple() { return kir_kind == Kind::Tuple; }
	};

	class PointerType : public Type
	{
		Type* kir_elem_t;
		PointerType(Type*);
	public:
		Type* getElementType() const;
		friend class Type;
	};

	class ArrayType : public Type
	{
		size_t arr_size;
		Type* kir_elem_t;
		ArrayType(Type*, size_t);
	public:
		Type* getElementType() const;
		size_t size() const;
		friend class Type;
	};

	class TupleType : public Type, public ExtensibleList<Type*>
	{
		TupleType(std::vector<Type*> types);
	public:
		friend class Type;
	};
}

#endif
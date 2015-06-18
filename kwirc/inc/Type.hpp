#ifndef KIWI_TYPE_HPP
#define KIWI_TYPE_HPP

#include "Utils.hpp"

namespace KIR
{
	class Type;
	class PointerType;
	class ArrayType;

	enum Kind : size_t
	{
		Void,
		Pointer,
		Bit,
		Integer,
		Float,
		Array,
		Tuple
	};

	enum Subkind : size_t
	{
		NA,
		U8, U16, U32, U64, U128,
		I8, I16, I32, I64, I128,
		FP32, FP64, FP80
	};

	class Type;
	bool operator==(const Type&, const Type&);

	class Type : public Object
	{
		Kind kir_kind;
		Subkind kir_subkind;
	protected:
		Type(Kind, Subkind = Subkind::NA);
		~Type() {}
	public:
		virtual size_t hash() const {
			return ::KIR::hash({static_cast<size_t>(kir_kind), static_cast<size_t>(kir_subkind)});
		}

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

		inline bool isNumber() const
		{
			return kir_kind == Kind::Integer || kir_kind == Kind::Float;
		}
		inline bool isInteger() const
		{
			return kir_kind == Kind::Integer;
		}
		inline bool isSignedInteger() const
		{
			if (kir_kind != Kind::Integer) return false;
			switch(kir_subkind)
			{
				case Subkind::I8:
				case Subkind::I16:
				case Subkind::I32:
				case Subkind::I64:
				case Subkind::I128:
				return true;
				default:
				return false;
			}
		}
		inline bool isUnsignedInteger() const
		{
			if (kir_kind != Kind::Integer) return false;
			switch(kir_kind)
			{
				case Subkind::U8:
				case Subkind::U16:
				case Subkind::U32:
				case Subkind::U64:
				case Subkind::U128:
				return true;
				default:
				return false;
			}
		}
		inline bool isFloat() const
		{
			if (kir_kind != Kind::Float) return false;
			switch(kir_subkind)
			{
				case Subkind::FP32:
				case Subkind::FP64:
				case Subkind::FP80:
				return true;
				default:
				return false;
			}
		}


		inline bool isVoid() const { return kir_kind == Kind::Void; }
		inline bool isPointer() const { return kir_kind == Kind::Pointer; }
		inline bool isBit() const { return kir_kind == Kind::Bit; }
		inline bool isI8() const { return kir_kind == Kind::Integer && kir_subkind == Subkind::I8; }
		inline bool isI16() const { return kir_kind == Kind::Integer && kir_subkind == Subkind::I16; }
		inline bool isI32() const { return kir_kind == Kind::Integer && kir_subkind == Subkind::I32; }
		inline bool isI64() const { return kir_kind == Kind::Integer && kir_subkind == Subkind::I64; }
		inline bool isI128() const { return kir_kind == Kind::Integer && kir_subkind == Subkind::I128; }
		inline bool isU8() const { return kir_kind == Kind::Integer && kir_subkind == Subkind::U8; }
		inline bool isU16() const { return kir_kind == Kind::Integer && kir_subkind == Subkind::U16; }
		inline bool isU32() const { return kir_kind == Kind::Integer && kir_subkind == Subkind::U32; }
		inline bool isU64() const { return kir_kind == Kind::Integer && kir_subkind == Subkind::U64; }
		inline bool isU128() const { return kir_kind == Kind::Integer && kir_subkind == Subkind::U128; }
		inline bool isFP32() const { return kir_kind == Kind::Float && kir_subkind == Subkind::FP32; }
		inline bool isFP64() const { return kir_kind == Kind::Float && kir_subkind == Subkind::FP64; }
		inline bool isFP80() const { return kir_kind == Kind::Float && kir_subkind == Subkind::FP80; }
		inline bool isArray() const { return kir_kind == Kind::Array; }
		inline bool isTuple() const { return kir_kind == Kind::Tuple; }
	};

	class PointerType : public Type
	{
		Type* const kir_elem_t;
		PointerType(Type*);
	public:
		Type* getElementType() const;
		friend class Type;

		virtual size_t hash() const
		{
			return ::KIR::hash({Type::hash(), kir_elem_t->hash()});
		}
	};

	class ArrayType : public Type
	{
		size_t arr_size;
		Type* const kir_elem_t;
		ArrayType(Type*, size_t);
	public:
		Type* getElementType() const;
		size_t size() const;
		friend class Type;

		virtual size_t hash() const
		{
			return ::KIR::hash({Type::hash(), kir_elem_t->hash(), arr_size});
		}
	};

	class TupleType : public Type, public ExtensibleList<Type*>
	{
		TupleType(std::vector<Type*> types);
	public:
		friend class Type;

		virtual size_t hash() const
		{
			std::vector<size_t> hashes;
			std::transform(begin(), end(), hashes.begin(), [](Type* const t) -> size_t { return t->hash(); });
			return ::KIR::hash(hashes.begin(), hashes.end());
		}
	};
}

namespace std
{
	template<>
	struct hash<KIR::Type*>
	{
		typedef KIR::Type* argument_type;
		typedef std::size_t result_type;

		result_type operator()(argument_type const& type) const
		{
			return type->hash();
		}
	};
}

#endif
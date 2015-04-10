#include <vector>
#include <stdio.h>

#include "objdef.hpp"

#ifndef KIWI_TYPE_HPP
#define KIWI_TYPE_HPP

namespace KIR
{
	class Type
	{
	public:
		virtual ~Type() = default;
		virtual bool isEqualTo(Type& rhs) = 0;
		bool operator==(Type& rhs);
		bool operator!=(Type& rhs);
	};

	bool Type::operator==(Type& rhs)
	{
		return this->isEqualTo(rhs);
	}

	bool Type::operator!=(Type& rhs)
	{
		return !(*this == rhs);
	}

	bool compareTypeVectors(std::vector<Type*> lhs, std::vector<Type*> rhs)
	{
		if (lhs.size() != rhs.size()) return false;
		for (int i = 0; i < lhs.size(); i++) if (*lhs[i] != *rhs[i]) return false;
		return true;
	}

	class TupleType: public Type
	{
		std::vector<Type*> types;
	public:
		TupleType(std::initializer_list<Type*> _types) : types(_types) {}
		~TupleType()
		{
			types.clear();
		}

		bool isEqualTo(Type& rhs)
		{
			TupleType* other = dynamic_cast<TupleType*>(&rhs);
			if (other != NULL)
				return compareTypeVectors(this->types, other->types);
			else
				return false;
		}

		Type* operator[](const size_t index) { return types[index]; }
		const size_t size() { return types.size(); }
	};

	enum struct IntegerFormat : int { I_1 = 1, I_8 = 8, I_16 = 16, I_32 = 32, I_64 = 64, I_128 = 128,
									  UI_8 = -8, UI_16 = -16, UI_32 = -32, UI_64 = -64, UI_128 = -128 };
	class IntegerType: public Type
	{
	public:
		IntegerFormat format;

		IntegerType(IntegerFormat _format) : format(_format) {}

		bool isEqualTo(Type& rhs)
		{
			IntegerType* other = dynamic_cast<IntegerType*>(&rhs);
			if (other != NULL)
				return this->format == other->format;
			else
				return false;
		}
	};

	enum struct FloatWidth : int { FP_16 = 16, FP_32 = 32, FP_64 = 64, FP_80 = 80 };
	class FloatType: public Type
	{
	public:
		FloatWidth width;

		FloatType(FloatWidth _width) : width(_width) {}

		bool isEqualTo(Type& rhs)
		{
			FloatType* other = dynamic_cast<FloatType*>(&rhs);
			if (other != NULL)
				return this->width == other->width;
			else
				return false;
		}
	};

	class StructType: public Type
	{
	public:
		StructDefinition* struct_def;
		StructType(StructDefinition* _struct_def) : struct_def(_struct_def) {}

		bool isEqualTo(Type& rhs)
		{
			StructType* other = dynamic_cast<StructType*>(&rhs);
			if (other != NULL)
				return this->struct_def == other->struct_def;
			else
				return false;
		}
	};

	class ClassType: public Type
	{
	public:
		ClassDefinition* class_def;
		ClassType(ClassDefinition* _class_def) : class_def(_class_def) {}

		bool isEqualTo(Type& rhs)
		{
			ClassType* other = dynamic_cast<ClassType*>(&rhs);
			if (other != NULL)
				return this->class_def == other->class_def;
			else
				return false;
		}
	};

	class PointerType: public Type
	{
		Type* pointer_to;
	public:
		PointerType(Type* _pointer_to) : pointer_to(_pointer_to) {}
		~PointerType() { delete pointer_to; }

		bool isEqualTo(Type& rhs)
		{
			PointerType* other = dynamic_cast<PointerType*>(&rhs);
			if (other != NULL)
				return *(this->pointer_to) == *(other->pointer_to);
			else
				return false;
		}
	};

	class ReferenceType: public Type
	{
		Type* reference_to;
	public:
		ReferenceType(Type* _reference_to) : reference_to(_reference_to) {}
		~ReferenceType() { delete reference_to; }

		bool isEqualTo(Type& rhs)
		{
			ReferenceType* other = dynamic_cast<ReferenceType*>(&rhs);
			if (other != NULL)
				return *(this->reference_to) == *(other->reference_to);
			else
				return false;
		}
	};

	class NullableType: public Type
	{
		Type* nullable_of;
	public:
		NullableType(Type* _nullable_of) : nullable_of(_nullable_of) {}
		~NullableType() { delete nullable_of; }

		bool isEqualTo(Type& rhs)
		{
			NullableType* other = dynamic_cast<NullableType*>(&rhs);
			if (other != NULL)
				return *(this->nullable_of) == *(other->nullable_of);
			else
				return false;
		}
	};

	class StaticArrayType: public Type
	{
		Type* elem_type;
		size_t size;
	public:
		StaticArrayType(Type* _elem_type, size_t _size) : elem_type(_elem_type), size(_size) {}
		~StaticArrayType() { delete elem_type; }

		bool isEqualTo(Type& rhs)
		{
			StaticArrayType* other = dynamic_cast<StaticArrayType*>(&rhs);
			if (other != NULL)
				return *(this->elem_type) == *(other->elem_type) && this->size == other->size;
			else
				return false;
		}
	};

	class FunctionType: public Type
	{
		TupleType args, rets;
	public:
		FunctionType(TupleType _args, TupleType _rets) : args(_args), rets(_rets) {}

		bool isEqualTo(Type& rhs)
		{
			FunctionType* other = dynamic_cast<FunctionType*>(&rhs);
			if (other != NULL)
				return this->args == other->args && this->rets == other->rets;
			else
				return false;
		}
	};

	class ClosureType: public FunctionType
	{
		ClosureType(TupleType _args, TupleType _rets) : FunctionType(_args, _rets) {}
	};
}

#endif
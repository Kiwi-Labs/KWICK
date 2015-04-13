#ifndef KIWI_TYPE_HPP
#define KIWI_TYPE_HPP

#include <vector>
#include <iostream>
#include <sstream>

#include "utils.hpp"

namespace KIR
{
	class Type: public Stringable, public Cloneable
	{
	public:
		virtual ~Type() {}
		virtual bool isEqualTo(Type& rhs) = 0;
		bool operator==(Type& rhs);
		bool operator!=(Type& rhs);
		bool assignableTo(Type& btype);
		virtual bool isAssignableTo(Type& btype) = 0;
		virtual Type* clone() = 0;
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

	class VoidType: public Type
	{
		bool isEqualTo(Type& rhs)
		{
			return false;
		}

		VoidType* clone() { return new VoidType(); }
		std::string toString()
		{
			return "Void";
		}

		bool isAssignableTo(Type& btype)
		{
			return false;
		}
	};

	class TupleType: public Type
	{
		std::vector<Type*> types;
	public:
		TupleType(std::initializer_list<Type*> _types) : types(_types) {}
		TupleType(std::vector<Type*> _types) : types(_types) {}
		~TupleType()
		{
			for (auto it = types.begin(); it < types.end(); it++)
				delete (*it);
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

		Type* collapseIfPossible()
		{
			if (types.size() == 1)
			{
				return types.back()->clone();
			}
			else if (types.size() == 0)
			{
				return new VoidType();
			}
			else
			{
				return this; // TODO: COLLAPSE ADDITIONAL TUPLES INTO THE BOTTOM TUPLE
			}
			return NULL;
		}

		Type*& operator[](const size_t index) { return types[index]; }
		const size_t size() { return types.size(); }

		TupleType* clone()
		{
			std::vector<Type*> ntypes;
			for (auto it = types.begin(); it < types.end(); it++)
			{
				ntypes.push_back((*it)->clone());
			}
			return new TupleType(ntypes);
		}

		std::string toString()
		{
			std::stringstream out;
			out << "(";
			if (types.size() == 0) return "()";
			out << types[0]->toString();
			for (auto it = types.begin()+1; it < types.end(); it++)
			{
				out << ", " << (*it)->toString();
			}
			out << ")";
			return out.str();
		}

		bool isAssignableTo(Type& btype)
		{
			if (TupleType* tt = dynamic_cast<TupleType*>(&btype)) return btype == *tt;
			else return *collapseIfPossible() == btype;
		}
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

		IntegerType* clone()
		{
			return new IntegerType(format);
		}

		std::string toString()
		{
			switch (format)
			{
				case IntegerFormat::I_1:
				return "Bit";
				case IntegerFormat::I_8:
				return "Byte";
				case IntegerFormat::I_16:
				return "Short";
				case IntegerFormat::I_32:
				return "Int";
				case IntegerFormat::I_64:
				return "Long";
				case IntegerFormat::I_128:
				return "Cent";
				case IntegerFormat::UI_8:
				return "UByte";
				case IntegerFormat::UI_16:
				return "UShort";
				case IntegerFormat::UI_32:
				return "UInt";
				case IntegerFormat::UI_64:
				return "ULong";
				case IntegerFormat::UI_128:
				return "UCent";
				default:
				return "WhatTheFuck";
			}
		}

		bool isAssignableTo(Type& btype)
		{
			return *this == btype;
		}
	};

	enum struct FloatFormat : int { FP_16 = 16, FP_32 = 32, FP_64 = 64, FP_80 = 80 };
	class FloatType: public Type
	{
	public:
		FloatFormat format;

		FloatType(FloatFormat _format) : format(_format) {}

		bool isEqualTo(Type& rhs)
		{
			FloatType* other = dynamic_cast<FloatType*>(&rhs);
			if (other != NULL)
				return this->format == other->format;
			else
				return false;
		}

		FloatType* clone()
		{
			return new FloatType(format);
		}

		std::string toString()
		{
			switch (format)
			{
				case FloatFormat::FP_16:
				return "Half";
				case FloatFormat::FP_32:
				return "Float";
				case FloatFormat::FP_64:
				return "Double";
				case FloatFormat::FP_80:
				return "Weird";
			}
		}

		bool isAssignableTo(Type& btype)
		{
			return *this == btype;
		}
	};

	class StructDefinition;
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

		StructType* clone()
		{
			return new StructType(struct_def);
		}

		std::string toString();

		bool isAssignableTo(Type& type);
	};

	class ClassDefinition;
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

		ClassType* clone()
		{
			return new ClassType(class_def);
		}

		std::string toString();

		bool isAssignableTo(Type& type);
	};

	class PointerType: public Type
	{
		Type* pointer_to;
	public:
		PointerType(Type* _pointer_to) : pointer_to(_pointer_to) {}
		~PointerType()
		{
			delete pointer_to;
		}

		bool isEqualTo(Type& rhs)
		{
			PointerType* other = dynamic_cast<PointerType*>(&rhs);
			if (other != NULL)
				return *(this->pointer_to) == *(other->pointer_to);
			else
				return false;
		}

		PointerType* clone()
		{
			return new PointerType(pointer_to->clone());
		}

		std::string toString()
		{
			return (std::stringstream() << pointer_to->toString() << "*").str();
		}

		bool isAssignableTo(Type& type)
		{
			return *this == type;
		}
	};

	class ReferenceType: public Type
	{
		Type* reference_to;
	public:
		ReferenceType(Type* _reference_to) : reference_to(_reference_to) {}
		~ReferenceType()
		{
			delete reference_to;
		}

		bool isEqualTo(Type& rhs)
		{
			ReferenceType* other = dynamic_cast<ReferenceType*>(&rhs);
			if (other != NULL)
				return *(this->reference_to) == *(other->reference_to);
			else
				return false;
		}

		ReferenceType* clone()
		{
			return new ReferenceType(reference_to->clone());
		}

		std::string toString()
		{
			return (std::stringstream() << reference_to->toString() << "!").str();
		}

		bool isAssignableTo(Type& type)
		{
			return *this == type;
		}
	};

	class NullableType: public Type
	{
		Type* nullable_of;
	public:
		NullableType(Type* _nullable_of) : nullable_of(_nullable_of) {}
		~NullableType()
		{
			delete nullable_of;
		}

		bool isEqualTo(Type& rhs)
		{
			NullableType* other = dynamic_cast<NullableType*>(&rhs);
			if (other != NULL)
				return *(this->nullable_of) == *(other->nullable_of);
			else
				return false;
		}

		NullableType* clone()
		{
			return new NullableType(nullable_of->clone());
		}

		std::string toString()
		{
			return (std::stringstream() << nullable_of->toString() << "?").str();
		}

		bool isAssignableTo(Type& type)
		{
			return *this == type;
		}
	};

	class StaticArrayType: public Type
	{
		Type* elem_type;
		size_t size;
	public:
		StaticArrayType(Type* _elem_type, size_t _size) : elem_type(_elem_type), size(_size) {}
		~StaticArrayType()
		{
			delete elem_type;
		}

		bool isEqualTo(Type& rhs)
		{
			StaticArrayType* other = dynamic_cast<StaticArrayType*>(&rhs);
			if (other != NULL)
				return *(this->elem_type) == *(other->elem_type) && this->size == other->size;
			else
				return false;
		}

		StaticArrayType* clone()
		{
			return new StaticArrayType(elem_type->clone(), size);
		}

		std::string toString()
		{
			return (std::stringstream() << elem_type->toString() << "[" << size << "]").str();
		}

		bool isAssignableTo(Type& type)
		{
			return *this == type;
		}
	};

	class FunctionType: public Type
	{
		TupleType* args;
		TupleType* rets;
	public:
		FunctionType(TupleType* _args, TupleType* _rets) : args(_args), rets(_rets) {}
		~FunctionType()
		{
			delete args;
			delete rets;
		}

		TupleType* getArgs() { return args->clone(); }
		TupleType* getRets() { return rets->clone(); }

		bool isEqualTo(Type& rhs)
		{
			FunctionType* other = dynamic_cast<FunctionType*>(&rhs);
			if (other != NULL)
				return static_cast<Type*>(this->args) == static_cast<Type*>(other->args) &&
					   static_cast<Type*>(this->rets) == static_cast<Type*>(other->rets);
			else
				return false;
		}

		FunctionType* clone()
		{
			return new FunctionType(args->clone(), rets->clone());
		}

		std::string toString()
		{
			return (std::stringstream() << "func" << args->toString() << " -> " << rets->toString()).str();
		}

		bool isAssignableTo(Type& type)
		{
			return *this == type;
		}
	};
}

#endif
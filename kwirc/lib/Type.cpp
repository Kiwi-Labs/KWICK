#ifndef KIWI_TYPE_CPP
#define KIWI_TYPE_CPP

#include "Type.hpp"
#include "Context.hpp"

#define T_MANAGED(x) getContext().t_mctx.add( (x) )

namespace KIR
{
	Type::Type(Kind _kir_kind, Subkind _kir_subkind) : kir_kind(_kir_kind), kir_subkind(_kir_subkind) {}

	Type* Type::getVoidType()
	{
		Context& k_ctx = getContext();
		MemoryContext<Type>& m_ctx = k_ctx.t_mctx;
		return m_ctx.add(new Type(Kind::Void));
	}
	Type* Type::getPtrType(Type* kir_elem_t) { return T_MANAGED(new PointerType(kir_elem_t)); }
	Type* Type::getBitType() { return T_MANAGED(new Type(Kind::Bit)); }
	Type* Type::getU8Type() { return T_MANAGED(new Type(Kind::Integer, Subkind::U8)); }
	Type* Type::getU16Type() { return T_MANAGED(new Type(Kind::Integer, Subkind::U16)); }
	Type* Type::getU32Type() { return T_MANAGED(new Type(Kind::Integer, Subkind::U32)); }
	Type* Type::getU64Type() { return T_MANAGED(new Type(Kind::Integer, Subkind::U64)); }
	Type* Type::getU128Type() { return T_MANAGED(new Type(Kind::Integer, Subkind::U128)); }
	Type* Type::getI8Type() { return T_MANAGED(new Type(Kind::Integer, Subkind::I8)); }
	Type* Type::getI16Type() { return T_MANAGED(new Type(Kind::Integer, Subkind::I16)); }
	Type* Type::getI32Type() { return T_MANAGED(new Type(Kind::Integer, Subkind::I32)); }
	Type* Type::getI64Type() { return T_MANAGED(new Type(Kind::Integer, Subkind::I64)); }
	Type* Type::getI128Type() { return T_MANAGED(new Type(Kind::Integer, Subkind::I128)); }
	Type* Type::getFP32Type() { return T_MANAGED(new Type(Kind::Float, Subkind::FP32)); }
	Type* Type::getFP64Type() { return T_MANAGED(new Type(Kind::Float, Subkind::FP64)); }
	Type* Type::getFP80Type() { return T_MANAGED(new Type(Kind::Float, Subkind::FP80)); }
	Type* Type::getTupleType(std::initializer_list<Type*> _init) { return T_MANAGED(new TupleType(_init)); }
	Type* Type::getTupleType(std::vector<Type*> _init) { return T_MANAGED(new TupleType(_init)); }

	bool operator==(const Type& t0, const Type& t1)
	{
		if (t0.kir_kind != t1.kir_kind || t0.kir_subkind != t1.kir_subkind) return false;
		else
		{
			switch(t0.kir_kind)
			{
			case Kind::Pointer:
				{
					const PointerType& ptr_t0 = static_cast<const PointerType&>(t0);
					const PointerType& ptr_t1 = static_cast<const PointerType&>(t1);
					return (*ptr_t0.getElementType()) == (*ptr_t1.getElementType());
				}
			case Kind::Array:
				{
					const ArrayType& arr_t0 = static_cast<const ArrayType&>(t0);
					const ArrayType& arr_t1 = static_cast<const ArrayType&>(t1);
					return (arr_t0.size() == arr_t1.size() && (*arr_t0.getElementType() == *arr_t1.getElementType()));
				}
			case Kind::Tuple:
				{
					const TupleType& tup_t0 = static_cast<const TupleType&>(t0);
					const TupleType& tup_t1 = static_cast<const TupleType&>(t1);
					return (std::equal(tup_t0.begin(), tup_t0.end(), tup_t1.begin(), [](Type* t0, Type* t1) { return (*t0) == (*t1); }));
				}
			default:
				return true;
			}
		}
	}

	PointerType::PointerType(Type* _kir_elem_t) : kir_elem_t(_kir_elem_t), Type(Kind::Pointer) {}
	Type* PointerType::getElementType() const { return kir_elem_t; }

	ArrayType::ArrayType(Type* _kir_elem_t, size_t _arr_size) : kir_elem_t(_kir_elem_t), arr_size(_arr_size), Type(Kind::Pointer) {}
	Type* ArrayType::getElementType() const { return kir_elem_t; }
	size_t ArrayType::size() const { return arr_size; }

	TupleType::TupleType(std::vector<Type*> types) : Type(Kind::Tuple), ExtensibleList<Type*>(types) {}
}

#endif
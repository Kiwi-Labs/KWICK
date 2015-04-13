#ifndef KIWI_OBJDEF_HPP
#define KIWI_OBJDEF_HPP

#include <unordered_map>

#include "module.hpp"
#include "symbol.hpp"
#include "statement.hpp"

namespace KIR
{
	class Accessible
	{
	public:
		virtual bool addBinding(SymbolIdentifier id, Binding* bd) = 0;
		virtual bool containsBinding(SymbolIdentifier id) = 0;
		virtual Binding* getBinding(SymbolIdentifier id) = 0;
		virtual Type* getBindingType(SymbolIdentifier id) = 0;
	};

	class StructDefinition: public Accessible, public Definition
	{
		SymbolIdentifier id;
		SymbolTable* symtab;
		StructDefinition* super;
		bool is_leaf = false;
		bool is_ref = false;
		StructDefinition(SymbolIdentifier _id, bool _is_ref = false, StructDefinition* _super = NULL) : id(_id), is_ref(_is_ref), super(_super) { if (super != NULL) symtab = new SymbolTable(super->symtab); }
	public:
		StructDefinition(SymbolIdentifier _id, bool _is_ref = false) : id(_id), is_ref(_is_ref), super(NULL), symtab(new SymbolTable()) {}
		bool addBinding(SymbolIdentifier _id, Binding* bd)
		{
			return symtab->setSymbol(_id, bd);
		}
		bool containsBinding(SymbolIdentifier _id)
		{
			return symtab->containsSymbol(_id);
		}
		Binding* getBinding(SymbolIdentifier _id)
		{
			return static_cast<Binding*>(symtab->getSymbol(_id));
		}
		Type* getBindingType(SymbolIdentifier _id)
		{
			Binding* bd = getBinding(_id);
			return (bd != NULL ? bd->getType() : NULL);
		}
		bool inheritsFrom(StructDefinition* other)
		{
			if (this == other) return true;
			else if (super != NULL) return super->inheritsFrom(other);
			else return false;
		}
		StructDefinition* createLeaf(SymbolIdentifier _id)
		{
			is_leaf = true;
			return new StructDefinition(_id, is_ref, this);
		}
		std::string toString()
		{
			std::stringstream out;
			if (is_ref) out << "ref ";
			out << "struct " << id << std::endl;
			out << "{" << std::endl;
			out << symtab->toString();
			out << "}" << std::endl;
			return out.str();
		}
	};

	class ClassDefinition: public Accessible, public Definition
	{
		SymbolIdentifier id;
		SymbolTable* symtab;
		ClassDefinition* super;
	public:
		ClassDefinition(SymbolIdentifier _id, ClassDefinition* super) : id(_id) { if (super != NULL) symtab = new SymbolTable(super->symtab); }
		ClassDefinition(SymbolIdentifier _id) : id(_id), symtab(new SymbolTable()) {}
		bool addBinding(SymbolIdentifier _id, Binding* bd)
		{
			return symtab->setSymbol(_id, bd);
		}
		bool containsBinding(SymbolIdentifier _id)
		{
			return symtab->containsSymbol(_id);
		}
		Binding* getBinding(SymbolIdentifier _id)
		{
			return static_cast<Binding*>(symtab->getSymbol(_id));
		}
		Type* getBindingType(SymbolIdentifier _id)
		{
			Binding* bd = getBinding(_id);
			return (bd != NULL ? bd->getType() : NULL);
		}
		bool inheritsFrom(ClassDefinition* other)
		{
			if (this == other) return true;
			else if (super != NULL) return super->inheritsFrom(other);
			else return false;
		}
		std::string toString()
		{
			std::stringstream out;
			out << "class " << id;
			if (super != NULL) out << " : " << super->id;
			out << std::endl << "{" << std::endl;
			out << symtab->toString();
			out << "}" << std::endl;
			return out.str();
		}
	};
}

#endif
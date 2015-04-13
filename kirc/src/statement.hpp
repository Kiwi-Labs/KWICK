#ifndef KIWI_STATEMENT_HPP
#define KIWI_STATEMENT_HPP

#include <iostream>
#include <sstream>

#include "utils.hpp"
#include "value.hpp"
#include "expr.hpp"
#include "block.hpp"

namespace KIR
{
	class Statement: public Stringable, public Cloneable
	{
		Block* parent;
		size_t loc;
	public:
		virtual ~Statement() {}
		friend class Block;
		virtual bool validate() = 0;
		virtual Statement* clone() = 0;
	};

	class Binding: public Value, public Definition, public Statement
	{
	private:
		bool mut; // Mutable?
	protected:
		Type* type;
		SymbolIdentifier ident;
	public:
		Binding(Type* _type, SymbolIdentifier _ident, bool _mut) : type(_type), ident(_ident), mut(_mut) {}
		~Binding()
		{
			delete type;
		}
		Type* getType()
		{
			return this->type;
		}
		SymbolIdentifier getIdentifier()
		{
			return this->ident;
		}
		Binding* clone()
		{
			return new Binding(type->clone(), ident, mut);
		}
		bool getMut()
		{
			return mut;
		}
		virtual bool validate() { return type != NULL; }
		virtual std::string toString()
		{
			return (std::stringstream() << (mut ? "var " : "let ") << ident << " : " << type->toString() << ";").str();
		}
	};

	class Accessor: public Binding
	{
	public:
		Function* getter;
		Function* setter;
		bool is_rma; /* Specifies whether the setter is a binding mutating accessor or a reference mutating accessor.
					    RMAs are passed a value type which is then mutated and returned and the original argument passed
					    is overwritten with the mutated version, and binding mutating accessors simply modify the value
					    directly as per the reference. */
		Accessor(Type* _type, SymbolIdentifier _ident, bool _is_rma, Function* _getter = NULL, Function* _setter = NULL)
			: Binding(_type, _ident, true), getter(_getter), setter(_setter), is_rma(_is_rma) {}
		bool validate()
		{
			bool valid = type != NULL;
			if (valid && getter) valid = *(getter->getType()->getRets()) == *type;
			if (valid && setter && is_rma) valid = *(setter->getType()->getRets()) == *(new VoidType());
			else if (valid && setter && !is_rma) valid = *(setter->getType()->getRets()) == *type;
			return valid;
		}
		std::string toString()
		{
			return (std::stringstream() << "accessor(" << (getter != NULL ? getter->getIdentifier() : "null") << ", " << (setter != NULL ? setter->getIdentifier() : "null") << ")" << ident << " : " << type->toString() << ";").str();
		}
	};

	class Assignment: public Statement
	{
		Binding* lhs;
		Expression* rhs;
		~Assignment()
		{
			delete lhs;
			delete rhs;
		}
	public:
		Assignment(Binding* _lhs, Expression* _rhs) : lhs(_lhs), rhs(_rhs) {}
		Assignment* clone()
		{
			return new Assignment(lhs->clone(), rhs->clone());
		}
		bool validate()
		{
			return *(lhs->getType()) == *(rhs->getType());
		}
		std::string toString()
		{
			return (std::stringstream() << lhs->getIdentifier() << " = (" << rhs->toString() << ")" << ";").str();
		}
	};

	class IfBranch: public Statement
	{
		Expression* cond;
		Block* true_block;
		Block* false_block;
	public:
		IfBranch(Expression* _cond, Block* _true_block, Block* _false_block = NULL) :
			cond(_cond), true_block(_true_block), false_block(_false_block) {}
		~IfBranch();
		Expression* getCondition() { return cond; }
		Block* getTrueBlock() { return true_block; }
		Block* getFalseBlock() { return false_block; }
		IfBranch* clone();
		bool validate();
		std::string toString();
	};
}

#endif
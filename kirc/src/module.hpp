#ifndef KIWI_MODULE_HPP
#define KIWI_MODULE_HPP

#include <string>
#include <vector>

#include "function.hpp"

namespace KIR
{
	// ---------------------------------------------------------------------------------------------------------------- //
	// KIR global context.

	class Module
	{
		std::string name;
		std::vector<Function*> functions;
	public:
		Module(std::string name);
	};
}

#endif
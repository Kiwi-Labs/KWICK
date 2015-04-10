#include <string>
#include <vector>

#include "function.hpp"

#ifndef KIWI_MODULE_HPP
#define KIWI_MODULE_HPP

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
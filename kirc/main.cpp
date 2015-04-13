#include <iostream>

#include "src/kir.hpp"

using namespace KIR;

int main(int argc, char* argv[])
{
	std::cout << "std::cout << \"std::cout << \"Hello world!\" << std::endl\" << std::endl;" << std::endl; // Because why not?

	Block* bb = new Block("entry");
	Binding* mybinding = new Binding(new IntegerType(IntegerFormat::I_8), "myvar", true);
	bb->pushStatement(mybinding);
	bb->pushStatement(new Assignment(mybinding, new IntegerLiteralExpression(23, IntegerFormat::I_16)));

	std::cout << bb->toString() << std::endl;
	return 0;
}
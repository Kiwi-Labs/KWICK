#include <stdio.h>

#include "src/kir.hpp"

int main(int argc, char* argv[])
{
	printf("Hello, world!\n");

	KIR::TupleType* tupleA = new KIR::TupleType({ new KIR::IntegerType(KIR::IntegerFormat::I_32), new KIR::IntegerType(KIR::IntegerFormat::I_32), new KIR::IntegerType(KIR::IntegerFormat::I_32) });
	KIR::TupleType* tupleB = new KIR::TupleType({ new KIR::IntegerType(KIR::IntegerFormat::I_32), new KIR::IntegerType(KIR::IntegerFormat::I_32), new KIR::IntegerType(KIR::IntegerFormat::I_32) });

	KIR::IntegerType* a;
	KIR::IntegerType* b;
	a = new KIR::IntegerType(KIR::IntegerFormat::I_32);
	b = new KIR::IntegerType(KIR::IntegerFormat::I_32);

	printf("Shit! %i\n", *tupleA == *tupleB);
	printf("Fuck! %i\n", *a == *b);

	delete tupleA;
	delete tupleB;
	delete a;
	delete b;

	return 0;
}
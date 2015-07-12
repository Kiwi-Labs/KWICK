# Kiwi

This is the home of the Kiwi programming language, which will eventually be implemented as a self-hosting compiler, but which must first be bootstrapped.  The project is currently in its early stages, and is being created in two parts.

## KWICK

The KiWi Initial Compiler for Kiwi is the temporary, bootstrapped front-end for the Kiwi compiler.  It is being implemented in Haskell, and will be rewritten in Kiwi once the initial compiler is complete.  KWICK is not a full, down-to-machine-code compiler, but instead works by emitting code in the Kiwi Intermediate Representation (KIR), to be processed by the back-end.

## KWIRC

The KiWi Intermediate Representation Compiler is the back-end of the Kiwi compiler.  It processes the KIR output of the front-end and leverages the LLVM to generate actual executable machine code.  This component will *not* be rewritten in Kiwi at a later date, and will instead be reused in its current form by the self-hosting front-end.

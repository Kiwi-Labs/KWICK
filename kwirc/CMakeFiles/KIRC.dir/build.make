# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.2

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:

# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list

# Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/local/Cellar/cmake/3.2.1/bin/cmake

# The command to remove a file.
RM = /usr/local/Cellar/cmake/3.2.1/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /Users/sean/Projects/KWICK/kirc

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /Users/sean/Projects/KWICK/kirc

# Include any dependencies generated for this target.
include CMakeFiles/KIRC.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/KIRC.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/KIRC.dir/flags.make

CMakeFiles/KIRC.dir/main.cpp.o: CMakeFiles/KIRC.dir/flags.make
CMakeFiles/KIRC.dir/main.cpp.o: main.cpp
	$(CMAKE_COMMAND) -E cmake_progress_report /Users/sean/Projects/KWICK/kirc/CMakeFiles $(CMAKE_PROGRESS_1)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building CXX object CMakeFiles/KIRC.dir/main.cpp.o"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++   $(CXX_DEFINES) $(CXX_FLAGS) -o CMakeFiles/KIRC.dir/main.cpp.o -c /Users/sean/Projects/KWICK/kirc/main.cpp

CMakeFiles/KIRC.dir/main.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/KIRC.dir/main.cpp.i"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -E /Users/sean/Projects/KWICK/kirc/main.cpp > CMakeFiles/KIRC.dir/main.cpp.i

CMakeFiles/KIRC.dir/main.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/KIRC.dir/main.cpp.s"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -S /Users/sean/Projects/KWICK/kirc/main.cpp -o CMakeFiles/KIRC.dir/main.cpp.s

CMakeFiles/KIRC.dir/main.cpp.o.requires:
.PHONY : CMakeFiles/KIRC.dir/main.cpp.o.requires

CMakeFiles/KIRC.dir/main.cpp.o.provides: CMakeFiles/KIRC.dir/main.cpp.o.requires
	$(MAKE) -f CMakeFiles/KIRC.dir/build.make CMakeFiles/KIRC.dir/main.cpp.o.provides.build
.PHONY : CMakeFiles/KIRC.dir/main.cpp.o.provides

CMakeFiles/KIRC.dir/main.cpp.o.provides.build: CMakeFiles/KIRC.dir/main.cpp.o

CMakeFiles/KIRC.dir/lib/Context.cpp.o: CMakeFiles/KIRC.dir/flags.make
CMakeFiles/KIRC.dir/lib/Context.cpp.o: lib/Context.cpp
	$(CMAKE_COMMAND) -E cmake_progress_report /Users/sean/Projects/KWICK/kirc/CMakeFiles $(CMAKE_PROGRESS_2)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building CXX object CMakeFiles/KIRC.dir/lib/Context.cpp.o"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++   $(CXX_DEFINES) $(CXX_FLAGS) -o CMakeFiles/KIRC.dir/lib/Context.cpp.o -c /Users/sean/Projects/KWICK/kirc/lib/Context.cpp

CMakeFiles/KIRC.dir/lib/Context.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/KIRC.dir/lib/Context.cpp.i"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -E /Users/sean/Projects/KWICK/kirc/lib/Context.cpp > CMakeFiles/KIRC.dir/lib/Context.cpp.i

CMakeFiles/KIRC.dir/lib/Context.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/KIRC.dir/lib/Context.cpp.s"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -S /Users/sean/Projects/KWICK/kirc/lib/Context.cpp -o CMakeFiles/KIRC.dir/lib/Context.cpp.s

CMakeFiles/KIRC.dir/lib/Context.cpp.o.requires:
.PHONY : CMakeFiles/KIRC.dir/lib/Context.cpp.o.requires

CMakeFiles/KIRC.dir/lib/Context.cpp.o.provides: CMakeFiles/KIRC.dir/lib/Context.cpp.o.requires
	$(MAKE) -f CMakeFiles/KIRC.dir/build.make CMakeFiles/KIRC.dir/lib/Context.cpp.o.provides.build
.PHONY : CMakeFiles/KIRC.dir/lib/Context.cpp.o.provides

CMakeFiles/KIRC.dir/lib/Context.cpp.o.provides.build: CMakeFiles/KIRC.dir/lib/Context.cpp.o

CMakeFiles/KIRC.dir/lib/Expression.cpp.o: CMakeFiles/KIRC.dir/flags.make
CMakeFiles/KIRC.dir/lib/Expression.cpp.o: lib/Expression.cpp
	$(CMAKE_COMMAND) -E cmake_progress_report /Users/sean/Projects/KWICK/kirc/CMakeFiles $(CMAKE_PROGRESS_3)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building CXX object CMakeFiles/KIRC.dir/lib/Expression.cpp.o"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++   $(CXX_DEFINES) $(CXX_FLAGS) -o CMakeFiles/KIRC.dir/lib/Expression.cpp.o -c /Users/sean/Projects/KWICK/kirc/lib/Expression.cpp

CMakeFiles/KIRC.dir/lib/Expression.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/KIRC.dir/lib/Expression.cpp.i"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -E /Users/sean/Projects/KWICK/kirc/lib/Expression.cpp > CMakeFiles/KIRC.dir/lib/Expression.cpp.i

CMakeFiles/KIRC.dir/lib/Expression.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/KIRC.dir/lib/Expression.cpp.s"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -S /Users/sean/Projects/KWICK/kirc/lib/Expression.cpp -o CMakeFiles/KIRC.dir/lib/Expression.cpp.s

CMakeFiles/KIRC.dir/lib/Expression.cpp.o.requires:
.PHONY : CMakeFiles/KIRC.dir/lib/Expression.cpp.o.requires

CMakeFiles/KIRC.dir/lib/Expression.cpp.o.provides: CMakeFiles/KIRC.dir/lib/Expression.cpp.o.requires
	$(MAKE) -f CMakeFiles/KIRC.dir/build.make CMakeFiles/KIRC.dir/lib/Expression.cpp.o.provides.build
.PHONY : CMakeFiles/KIRC.dir/lib/Expression.cpp.o.provides

CMakeFiles/KIRC.dir/lib/Expression.cpp.o.provides.build: CMakeFiles/KIRC.dir/lib/Expression.cpp.o

CMakeFiles/KIRC.dir/lib/Type.cpp.o: CMakeFiles/KIRC.dir/flags.make
CMakeFiles/KIRC.dir/lib/Type.cpp.o: lib/Type.cpp
	$(CMAKE_COMMAND) -E cmake_progress_report /Users/sean/Projects/KWICK/kirc/CMakeFiles $(CMAKE_PROGRESS_4)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building CXX object CMakeFiles/KIRC.dir/lib/Type.cpp.o"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++   $(CXX_DEFINES) $(CXX_FLAGS) -o CMakeFiles/KIRC.dir/lib/Type.cpp.o -c /Users/sean/Projects/KWICK/kirc/lib/Type.cpp

CMakeFiles/KIRC.dir/lib/Type.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/KIRC.dir/lib/Type.cpp.i"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -E /Users/sean/Projects/KWICK/kirc/lib/Type.cpp > CMakeFiles/KIRC.dir/lib/Type.cpp.i

CMakeFiles/KIRC.dir/lib/Type.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/KIRC.dir/lib/Type.cpp.s"
	/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -S /Users/sean/Projects/KWICK/kirc/lib/Type.cpp -o CMakeFiles/KIRC.dir/lib/Type.cpp.s

CMakeFiles/KIRC.dir/lib/Type.cpp.o.requires:
.PHONY : CMakeFiles/KIRC.dir/lib/Type.cpp.o.requires

CMakeFiles/KIRC.dir/lib/Type.cpp.o.provides: CMakeFiles/KIRC.dir/lib/Type.cpp.o.requires
	$(MAKE) -f CMakeFiles/KIRC.dir/build.make CMakeFiles/KIRC.dir/lib/Type.cpp.o.provides.build
.PHONY : CMakeFiles/KIRC.dir/lib/Type.cpp.o.provides

CMakeFiles/KIRC.dir/lib/Type.cpp.o.provides.build: CMakeFiles/KIRC.dir/lib/Type.cpp.o

# Object files for target KIRC
KIRC_OBJECTS = \
"CMakeFiles/KIRC.dir/main.cpp.o" \
"CMakeFiles/KIRC.dir/lib/Context.cpp.o" \
"CMakeFiles/KIRC.dir/lib/Expression.cpp.o" \
"CMakeFiles/KIRC.dir/lib/Type.cpp.o"

# External object files for target KIRC
KIRC_EXTERNAL_OBJECTS =

KIRC: CMakeFiles/KIRC.dir/main.cpp.o
KIRC: CMakeFiles/KIRC.dir/lib/Context.cpp.o
KIRC: CMakeFiles/KIRC.dir/lib/Expression.cpp.o
KIRC: CMakeFiles/KIRC.dir/lib/Type.cpp.o
KIRC: CMakeFiles/KIRC.dir/build.make
KIRC: /usr/local/lib/libLLVMSupport.a
KIRC: /usr/local/lib/libLLVMTableGen.a
KIRC: /usr/local/lib/libLLVMCore.a
KIRC: /usr/local/lib/libLLVMIRReader.a
KIRC: /usr/local/lib/libLLVMCodeGen.a
KIRC: /usr/local/lib/libLLVMSelectionDAG.a
KIRC: /usr/local/lib/libLLVMAsmPrinter.a
KIRC: /usr/local/lib/libLLVMBitReader.a
KIRC: /usr/local/lib/libLLVMBitWriter.a
KIRC: /usr/local/lib/libLLVMTransformUtils.a
KIRC: /usr/local/lib/libLLVMInstrumentation.a
KIRC: /usr/local/lib/libLLVMInstCombine.a
KIRC: /usr/local/lib/libLLVMScalarOpts.a
KIRC: /usr/local/lib/libLLVMipo.a
KIRC: /usr/local/lib/libLLVMVectorize.a
KIRC: /usr/local/lib/libLLVMObjCARCOpts.a
KIRC: /usr/local/lib/libLLVMLinker.a
KIRC: /usr/local/lib/libLLVMAnalysis.a
KIRC: /usr/local/lib/libLLVMipa.a
KIRC: /usr/local/lib/libLLVMLTO.a
KIRC: /usr/local/lib/libLLVMMC.a
KIRC: /usr/local/lib/libLLVMMCParser.a
KIRC: /usr/local/lib/libLLVMMCDisassembler.a
KIRC: /usr/local/lib/libLLVMObject.a
KIRC: /usr/local/lib/libLLVMOption.a
KIRC: /usr/local/lib/libLLVMDebugInfo.a
KIRC: /usr/local/lib/libLLVMExecutionEngine.a
KIRC: /usr/local/lib/libLLVMInterpreter.a
KIRC: /usr/local/lib/libLLVMMCJIT.a
KIRC: /usr/local/lib/libLLVMRuntimeDyld.a
KIRC: /usr/local/lib/libLLVMTarget.a
KIRC: /usr/local/lib/libLLVMAArch64CodeGen.a
KIRC: /usr/local/lib/libLLVMAArch64Info.a
KIRC: /usr/local/lib/libLLVMAArch64AsmParser.a
KIRC: /usr/local/lib/libLLVMAArch64Disassembler.a
KIRC: /usr/local/lib/libLLVMAArch64AsmPrinter.a
KIRC: /usr/local/lib/libLLVMAArch64Desc.a
KIRC: /usr/local/lib/libLLVMAArch64Utils.a
KIRC: /usr/local/lib/libLLVMARMCodeGen.a
KIRC: /usr/local/lib/libLLVMARMInfo.a
KIRC: /usr/local/lib/libLLVMARMAsmParser.a
KIRC: /usr/local/lib/libLLVMARMDisassembler.a
KIRC: /usr/local/lib/libLLVMARMAsmPrinter.a
KIRC: /usr/local/lib/libLLVMARMDesc.a
KIRC: /usr/local/lib/libLLVMCppBackendCodeGen.a
KIRC: /usr/local/lib/libLLVMCppBackendInfo.a
KIRC: /usr/local/lib/libLLVMHexagonCodeGen.a
KIRC: /usr/local/lib/libLLVMHexagonInfo.a
KIRC: /usr/local/lib/libLLVMHexagonDesc.a
KIRC: /usr/local/lib/libLLVMHexagonDisassembler.a
KIRC: /usr/local/lib/libLLVMMipsCodeGen.a
KIRC: /usr/local/lib/libLLVMMipsAsmPrinter.a
KIRC: /usr/local/lib/libLLVMMipsDisassembler.a
KIRC: /usr/local/lib/libLLVMMipsInfo.a
KIRC: /usr/local/lib/libLLVMMipsDesc.a
KIRC: /usr/local/lib/libLLVMMipsAsmParser.a
KIRC: /usr/local/lib/libLLVMMSP430CodeGen.a
KIRC: /usr/local/lib/libLLVMMSP430AsmPrinter.a
KIRC: /usr/local/lib/libLLVMMSP430Info.a
KIRC: /usr/local/lib/libLLVMMSP430Desc.a
KIRC: /usr/local/lib/libLLVMNVPTXCodeGen.a
KIRC: /usr/local/lib/libLLVMNVPTXInfo.a
KIRC: /usr/local/lib/libLLVMNVPTXAsmPrinter.a
KIRC: /usr/local/lib/libLLVMNVPTXDesc.a
KIRC: /usr/local/lib/libLLVMPowerPCCodeGen.a
KIRC: /usr/local/lib/libLLVMPowerPCAsmParser.a
KIRC: /usr/local/lib/libLLVMPowerPCDisassembler.a
KIRC: /usr/local/lib/libLLVMPowerPCAsmPrinter.a
KIRC: /usr/local/lib/libLLVMPowerPCInfo.a
KIRC: /usr/local/lib/libLLVMPowerPCDesc.a
KIRC: /usr/local/lib/libLLVMR600CodeGen.a
KIRC: /usr/local/lib/libLLVMR600AsmParser.a
KIRC: /usr/local/lib/libLLVMR600AsmPrinter.a
KIRC: /usr/local/lib/libLLVMR600Info.a
KIRC: /usr/local/lib/libLLVMR600Desc.a
KIRC: /usr/local/lib/libLLVMSparcCodeGen.a
KIRC: /usr/local/lib/libLLVMSparcInfo.a
KIRC: /usr/local/lib/libLLVMSparcDesc.a
KIRC: /usr/local/lib/libLLVMSparcAsmPrinter.a
KIRC: /usr/local/lib/libLLVMSparcAsmParser.a
KIRC: /usr/local/lib/libLLVMSparcDisassembler.a
KIRC: /usr/local/lib/libLLVMSystemZCodeGen.a
KIRC: /usr/local/lib/libLLVMSystemZAsmParser.a
KIRC: /usr/local/lib/libLLVMSystemZDisassembler.a
KIRC: /usr/local/lib/libLLVMSystemZAsmPrinter.a
KIRC: /usr/local/lib/libLLVMSystemZInfo.a
KIRC: /usr/local/lib/libLLVMSystemZDesc.a
KIRC: /usr/local/lib/libLLVMX86CodeGen.a
KIRC: /usr/local/lib/libLLVMX86AsmParser.a
KIRC: /usr/local/lib/libLLVMX86Disassembler.a
KIRC: /usr/local/lib/libLLVMX86AsmPrinter.a
KIRC: /usr/local/lib/libLLVMX86Desc.a
KIRC: /usr/local/lib/libLLVMX86Info.a
KIRC: /usr/local/lib/libLLVMX86Utils.a
KIRC: /usr/local/lib/libLLVMXCoreCodeGen.a
KIRC: /usr/local/lib/libLLVMXCoreDisassembler.a
KIRC: /usr/local/lib/libLLVMXCoreAsmPrinter.a
KIRC: /usr/local/lib/libLLVMXCoreInfo.a
KIRC: /usr/local/lib/libLLVMXCoreDesc.a
KIRC: /usr/local/lib/libLLVMAsmParser.a
KIRC: /usr/local/lib/libLLVMLineEditor.a
KIRC: /usr/local/lib/libLLVMProfileData.a
KIRC: /usr/local/lib/libgtest.a
KIRC: /usr/local/lib/libgtest_main.a
KIRC: /usr/local/lib/libLTO.dylib
KIRC: /usr/local/lib/libLLVMBitWriter.a
KIRC: /usr/local/lib/libLLVMObjCARCOpts.a
KIRC: /usr/local/lib/libLLVMLinker.a
KIRC: /usr/local/lib/libLLVMExecutionEngine.a
KIRC: /usr/local/lib/libLLVMRuntimeDyld.a
KIRC: /usr/local/lib/libLLVMAArch64AsmPrinter.a
KIRC: /usr/local/lib/libLLVMAArch64Info.a
KIRC: /usr/local/lib/libLLVMAArch64Utils.a
KIRC: /usr/local/lib/libLLVMARMInfo.a
KIRC: /usr/local/lib/libLLVMARMAsmPrinter.a
KIRC: /usr/local/lib/libLLVMHexagonDesc.a
KIRC: /usr/local/lib/libLLVMHexagonInfo.a
KIRC: /usr/local/lib/libLLVMMipsDesc.a
KIRC: /usr/local/lib/libLLVMMipsAsmPrinter.a
KIRC: /usr/local/lib/libLLVMMipsInfo.a
KIRC: /usr/local/lib/libLLVMMSP430AsmPrinter.a
KIRC: /usr/local/lib/libLLVMMSP430Info.a
KIRC: /usr/local/lib/libLLVMNVPTXInfo.a
KIRC: /usr/local/lib/libLLVMNVPTXAsmPrinter.a
KIRC: /usr/local/lib/libLLVMPowerPCAsmPrinter.a
KIRC: /usr/local/lib/libLLVMPowerPCInfo.a
KIRC: /usr/local/lib/libLLVMipo.a
KIRC: /usr/local/lib/libLLVMVectorize.a
KIRC: /usr/local/lib/libLLVMR600AsmPrinter.a
KIRC: /usr/local/lib/libLLVMR600Info.a
KIRC: /usr/local/lib/libLLVMSparcDesc.a
KIRC: /usr/local/lib/libLLVMSparcAsmPrinter.a
KIRC: /usr/local/lib/libLLVMSparcInfo.a
KIRC: /usr/local/lib/libLLVMSystemZAsmPrinter.a
KIRC: /usr/local/lib/libLLVMSystemZInfo.a
KIRC: /usr/local/lib/libLLVMX86CodeGen.a
KIRC: /usr/local/lib/libLLVMX86Desc.a
KIRC: /usr/local/lib/libLLVMX86AsmPrinter.a
KIRC: /usr/local/lib/libLLVMX86Utils.a
KIRC: /usr/local/lib/libLLVMX86Info.a
KIRC: /usr/local/lib/libLLVMSelectionDAG.a
KIRC: /usr/local/lib/libLLVMAsmPrinter.a
KIRC: /usr/local/lib/libLLVMCodeGen.a
KIRC: /usr/local/lib/libLLVMScalarOpts.a
KIRC: /usr/local/lib/libLLVMInstCombine.a
KIRC: /usr/local/lib/libLLVMProfileData.a
KIRC: /usr/local/lib/libLLVMObject.a
KIRC: /usr/local/lib/libLLVMBitReader.a
KIRC: /usr/local/lib/libLLVMMCParser.a
KIRC: /usr/local/lib/libLLVMTransformUtils.a
KIRC: /usr/local/lib/libLLVMipa.a
KIRC: /usr/local/lib/libLLVMAnalysis.a
KIRC: /usr/local/lib/libLLVMTarget.a
KIRC: /usr/local/lib/libLLVMCore.a
KIRC: /usr/local/lib/libLLVMXCoreAsmPrinter.a
KIRC: /usr/local/lib/libLLVMMCDisassembler.a
KIRC: /usr/local/lib/libLLVMMC.a
KIRC: /usr/local/lib/libLLVMXCoreInfo.a
KIRC: /usr/local/lib/libgtest.a
KIRC: /usr/local/lib/libLLVMSupport.a
KIRC: CMakeFiles/KIRC.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --red --bold "Linking CXX executable KIRC"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/KIRC.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/KIRC.dir/build: KIRC
.PHONY : CMakeFiles/KIRC.dir/build

CMakeFiles/KIRC.dir/requires: CMakeFiles/KIRC.dir/main.cpp.o.requires
CMakeFiles/KIRC.dir/requires: CMakeFiles/KIRC.dir/lib/Context.cpp.o.requires
CMakeFiles/KIRC.dir/requires: CMakeFiles/KIRC.dir/lib/Expression.cpp.o.requires
CMakeFiles/KIRC.dir/requires: CMakeFiles/KIRC.dir/lib/Type.cpp.o.requires
.PHONY : CMakeFiles/KIRC.dir/requires

CMakeFiles/KIRC.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/KIRC.dir/cmake_clean.cmake
.PHONY : CMakeFiles/KIRC.dir/clean

CMakeFiles/KIRC.dir/depend:
	cd /Users/sean/Projects/KWICK/kirc && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /Users/sean/Projects/KWICK/kirc /Users/sean/Projects/KWICK/kirc /Users/sean/Projects/KWICK/kirc /Users/sean/Projects/KWICK/kirc /Users/sean/Projects/KWICK/kirc/CMakeFiles/KIRC.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/KIRC.dir/depend

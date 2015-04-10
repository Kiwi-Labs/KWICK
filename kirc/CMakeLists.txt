cmake_minimum_required (VERSION 3.0.2)
project (KIRC)
# Kiwi IR Compiler. To be linked in as a dynamically linked library, but starting as an executable for testing.
set (KIRC_VERSION_MAJOR 1)
set (KIRC_VERSION_MINOR 0)

set (KIRC_SOURCES main.cpp)

add_executable(KIRC main.cpp)

add_custom_command(
  OUTPUT runtimeoutput
  COMMAND KIRC
  COMMENT "Running built executable"
  VERBATIM
)

add_custom_target(
  KIRC_run ALL
  DEPENDS runtimeoutput
)
# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.11

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


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
CMAKE_COMMAND = /Applications/CMake.app/Contents/bin/cmake

# The command to remove a file.
RM = /Applications/CMake.app/Contents/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /Users/rbc-laptop/Documents/GitHub/ListsTest

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux

# Include any dependencies generated for this target.
include src/lib/CMakeFiles/ListsTestLib.dir/depend.make

# Include the progress variables for this target.
include src/lib/CMakeFiles/ListsTestLib.dir/progress.make

# Include the compile flags for this target's objects.
include src/lib/CMakeFiles/ListsTestLib.dir/flags.make

src/lib/CMakeFiles/ListsTestLib.dir/abstract_list.f90.o: src/lib/CMakeFiles/ListsTestLib.dir/flags.make
src/lib/CMakeFiles/ListsTestLib.dir/abstract_list.f90.o: ../src/lib/abstract_list.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building Fortran object src/lib/CMakeFiles/ListsTestLib.dir/abstract_list.f90.o"
	cd /Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux/src/lib && /Users/rbc-laptop/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/rbc-laptop/Documents/GitHub/ListsTest/src/lib/abstract_list.f90 -o CMakeFiles/ListsTestLib.dir/abstract_list.f90.o

src/lib/CMakeFiles/ListsTestLib.dir/abstract_list.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/ListsTestLib.dir/abstract_list.f90.i"
	cd /Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux/src/lib && /Users/rbc-laptop/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/rbc-laptop/Documents/GitHub/ListsTest/src/lib/abstract_list.f90 > CMakeFiles/ListsTestLib.dir/abstract_list.f90.i

src/lib/CMakeFiles/ListsTestLib.dir/abstract_list.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/ListsTestLib.dir/abstract_list.f90.s"
	cd /Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux/src/lib && /Users/rbc-laptop/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/rbc-laptop/Documents/GitHub/ListsTest/src/lib/abstract_list.f90 -o CMakeFiles/ListsTestLib.dir/abstract_list.f90.s

src/lib/CMakeFiles/ListsTestLib.dir/integer_list.f90.o: src/lib/CMakeFiles/ListsTestLib.dir/flags.make
src/lib/CMakeFiles/ListsTestLib.dir/integer_list.f90.o: ../src/lib/integer_list.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building Fortran object src/lib/CMakeFiles/ListsTestLib.dir/integer_list.f90.o"
	cd /Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux/src/lib && /Users/rbc-laptop/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/rbc-laptop/Documents/GitHub/ListsTest/src/lib/integer_list.f90 -o CMakeFiles/ListsTestLib.dir/integer_list.f90.o

src/lib/CMakeFiles/ListsTestLib.dir/integer_list.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/ListsTestLib.dir/integer_list.f90.i"
	cd /Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux/src/lib && /Users/rbc-laptop/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/rbc-laptop/Documents/GitHub/ListsTest/src/lib/integer_list.f90 > CMakeFiles/ListsTestLib.dir/integer_list.f90.i

src/lib/CMakeFiles/ListsTestLib.dir/integer_list.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/ListsTestLib.dir/integer_list.f90.s"
	cd /Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux/src/lib && /Users/rbc-laptop/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/rbc-laptop/Documents/GitHub/ListsTest/src/lib/integer_list.f90 -o CMakeFiles/ListsTestLib.dir/integer_list.f90.s

src/lib/CMakeFiles/ListsTestLib.dir/link.f90.o: src/lib/CMakeFiles/ListsTestLib.dir/flags.make
src/lib/CMakeFiles/ListsTestLib.dir/link.f90.o: ../src/lib/link.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Building Fortran object src/lib/CMakeFiles/ListsTestLib.dir/link.f90.o"
	cd /Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux/src/lib && /Users/rbc-laptop/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/rbc-laptop/Documents/GitHub/ListsTest/src/lib/link.f90 -o CMakeFiles/ListsTestLib.dir/link.f90.o

src/lib/CMakeFiles/ListsTestLib.dir/link.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/ListsTestLib.dir/link.f90.i"
	cd /Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux/src/lib && /Users/rbc-laptop/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/rbc-laptop/Documents/GitHub/ListsTest/src/lib/link.f90 > CMakeFiles/ListsTestLib.dir/link.f90.i

src/lib/CMakeFiles/ListsTestLib.dir/link.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/ListsTestLib.dir/link.f90.s"
	cd /Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux/src/lib && /Users/rbc-laptop/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/rbc-laptop/Documents/GitHub/ListsTest/src/lib/link.f90 -o CMakeFiles/ListsTestLib.dir/link.f90.s

src/lib/CMakeFiles/ListsTestLib.dir/list.f90.o: src/lib/CMakeFiles/ListsTestLib.dir/flags.make
src/lib/CMakeFiles/ListsTestLib.dir/list.f90.o: ../src/lib/list.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Building Fortran object src/lib/CMakeFiles/ListsTestLib.dir/list.f90.o"
	cd /Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux/src/lib && /Users/rbc-laptop/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/rbc-laptop/Documents/GitHub/ListsTest/src/lib/list.f90 -o CMakeFiles/ListsTestLib.dir/list.f90.o

src/lib/CMakeFiles/ListsTestLib.dir/list.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/ListsTestLib.dir/list.f90.i"
	cd /Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux/src/lib && /Users/rbc-laptop/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/rbc-laptop/Documents/GitHub/ListsTest/src/lib/list.f90 > CMakeFiles/ListsTestLib.dir/list.f90.i

src/lib/CMakeFiles/ListsTestLib.dir/list.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/ListsTestLib.dir/list.f90.s"
	cd /Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux/src/lib && /Users/rbc-laptop/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/rbc-laptop/Documents/GitHub/ListsTest/src/lib/list.f90 -o CMakeFiles/ListsTestLib.dir/list.f90.s

src/lib/CMakeFiles/ListsTestLib.dir/types.f90.o: src/lib/CMakeFiles/ListsTestLib.dir/flags.make
src/lib/CMakeFiles/ListsTestLib.dir/types.f90.o: ../src/lib/types.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_5) "Building Fortran object src/lib/CMakeFiles/ListsTestLib.dir/types.f90.o"
	cd /Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux/src/lib && /Users/rbc-laptop/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/rbc-laptop/Documents/GitHub/ListsTest/src/lib/types.f90 -o CMakeFiles/ListsTestLib.dir/types.f90.o

src/lib/CMakeFiles/ListsTestLib.dir/types.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/ListsTestLib.dir/types.f90.i"
	cd /Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux/src/lib && /Users/rbc-laptop/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/rbc-laptop/Documents/GitHub/ListsTest/src/lib/types.f90 > CMakeFiles/ListsTestLib.dir/types.f90.i

src/lib/CMakeFiles/ListsTestLib.dir/types.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/ListsTestLib.dir/types.f90.s"
	cd /Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux/src/lib && /Users/rbc-laptop/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/rbc-laptop/Documents/GitHub/ListsTest/src/lib/types.f90 -o CMakeFiles/ListsTestLib.dir/types.f90.s

# Object files for target ListsTestLib
ListsTestLib_OBJECTS = \
"CMakeFiles/ListsTestLib.dir/abstract_list.f90.o" \
"CMakeFiles/ListsTestLib.dir/integer_list.f90.o" \
"CMakeFiles/ListsTestLib.dir/link.f90.o" \
"CMakeFiles/ListsTestLib.dir/list.f90.o" \
"CMakeFiles/ListsTestLib.dir/types.f90.o"

# External object files for target ListsTestLib
ListsTestLib_EXTERNAL_OBJECTS =

lib/libListsTestLib.a: src/lib/CMakeFiles/ListsTestLib.dir/abstract_list.f90.o
lib/libListsTestLib.a: src/lib/CMakeFiles/ListsTestLib.dir/integer_list.f90.o
lib/libListsTestLib.a: src/lib/CMakeFiles/ListsTestLib.dir/link.f90.o
lib/libListsTestLib.a: src/lib/CMakeFiles/ListsTestLib.dir/list.f90.o
lib/libListsTestLib.a: src/lib/CMakeFiles/ListsTestLib.dir/types.f90.o
lib/libListsTestLib.a: src/lib/CMakeFiles/ListsTestLib.dir/build.make
lib/libListsTestLib.a: src/lib/CMakeFiles/ListsTestLib.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_6) "Linking Fortran static library ../../lib/libListsTestLib.a"
	cd /Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux/src/lib && $(CMAKE_COMMAND) -P CMakeFiles/ListsTestLib.dir/cmake_clean_target.cmake
	cd /Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux/src/lib && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/ListsTestLib.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
src/lib/CMakeFiles/ListsTestLib.dir/build: lib/libListsTestLib.a

.PHONY : src/lib/CMakeFiles/ListsTestLib.dir/build

src/lib/CMakeFiles/ListsTestLib.dir/clean:
	cd /Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux/src/lib && $(CMAKE_COMMAND) -P CMakeFiles/ListsTestLib.dir/cmake_clean.cmake
.PHONY : src/lib/CMakeFiles/ListsTestLib.dir/clean

src/lib/CMakeFiles/ListsTestLib.dir/depend:
	cd /Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /Users/rbc-laptop/Documents/GitHub/ListsTest /Users/rbc-laptop/Documents/GitHub/ListsTest/src/lib /Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux /Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux/src/lib /Users/rbc-laptop/Documents/GitHub/ListsTest/build_linux/src/lib/CMakeFiles/ListsTestLib.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : src/lib/CMakeFiles/ListsTestLib.dir/depend


cmake_minimum_required(VERSION 3.16)
project(Compiler01 VERSION 0.1.0)

set(CMAKE_CXX_FLAGS "-m64 -fno-exceptions -fno-rtti -Wall -Wextra -std=c++20")
set(CMAKE_CXX_DEBUG_FLAGS "-g")
set(CMAKE_CXX_RELEASE_FLAGS "-O3 -march=native")

set(SOURCE_FILES
        src/Main.cpp
        src/Compiler.cpp

        src/misc/Logging.cpp
        src/misc/SourceFile.cpp
        src/misc/StringPool.cpp
        src/misc/GetOpt.cpp
        src/misc/Terminal.cpp

        src/lexing/Lexer.cpp

        src/parsing/Parser.cpp
        src/misc/TokenTypes.cpp src/misc/TokenTypes.hpp)

add_executable(Compiler01 ${SOURCE_FILES})

target_include_directories(Compiler01 PUBLIC "src")
target_include_directories(Compiler01 PUBLIC "extlibs/include")

target_link_libraries(Compiler01 PUBLIC "C:/Users/Jaakko/source/repos/VSC Repos/Compiler01/extlibs/bin/libfmt.a")
#pragma once

#include <stdint.h>

inline constexpr unsigned GETOPT_SUCCESS = 0;
// GETOPT_ERR_INVALID will be returned when the value of the requested parameter
// cannot be parsed to the required type. E.g. "hello" cannot be parsed to a bool.
inline constexpr unsigned GETOPT_ERR_INVALID = 1;
// GETOPT_ERR_NOT_FOUND is returned if none the required option(s) were found.
inline constexpr unsigned GETOPT_ERR_NOT_FOUND = 2;
// GETOPT_ERR_INVALID_PARAMS will be returned if either any of the input pointers
// or any values inside the 'argv' array are null.
inline constexpr unsigned GETOPT_ERR_INVALID_PARAMS = 3;
// GETOPT_ERR_MULTIPLE will be returned if multiple option with same name (or alias)
// were found. For example "-o=5 -o=6" will be deemed invalid.
inline constexpr unsigned GETOPT_ERR_MULTIPLE = 4;


bool hasOpt(int* argc, char** argv, const char* key);

bool getOptName(const char* arg, char* output, size_t outputLength);

unsigned getOpt(int* argc, char** argv, const char* key, char const** out);

unsigned getOptBool(int* argc, char** argv, const char* key, bool* out);

unsigned getOptInt(int* argc, char** argv, const char* key, int32_t* out);

unsigned getOptInt64(int* argc, char** argv, const char* key, int64_t* out);



inline unsigned getOpt(int* argc, char** argv, const char* key, char** out) {
	return getOpt(argc, argv, key, const_cast<const char**>(out));
}
#include "GetOpt.hpp"

#include <string.h>

static void removeArgument(int* argc, char** argv, int index) {
	int count = *argc;
	*argc = count - 1;

	for (int i = index+1; i < count; ++i) {
		argv[i - 1] = argv[i];
	}
	argv[count] = nullptr;
}

// Called with a single key rather than a possible bundle of keys
// No safety checks, getOpt should be called instead
static unsigned getOptRaw(int* argc, char** argv, const char* key, size_t keyLength, const char** out) {
	int count = *argc; // safe because removeArgument() is only called just before returning

	char** argument = argv;
	for (int i = 1; i < count; ++i) { // i=1: skip executable name
		char* str = *argument++;
		if (!str) {
			return GETOPT_ERR_INVALID_PARAMS;
		}
		if (*str != '-') {
			continue;
		}
		str++; // skip '-'

		size_t argLength = strlen(str);

		if (strncmp(str, key, keyLength) == 0) {
			if (str[keyLength] == '=') { // Key-value pair is of form '-key=value'
				*out = str + keyLength + 1; // Return substring. +1 to skip '='
				removeArgument(argc, argv, i);
				return GETOPT_SUCCESS;
			}
			// Key-value pair is of form '-key value' (without =)
			if (i >= count - 1) { // if this was the last argument
				break;
			}

			char* next = *argument;
			if (*next != '-') { // if not a flag, return the next argument as the value
				*out = next;
				removeArgument(argc, argv, i - 1); // key
				removeArgument(argc, argv, i); // value
				return GETOPT_SUCCESS;
			}
		}
	}
	return GETOPT_ERR_NOT_FOUND;
}

// TODO report error if multiple matches (duplicates)?
unsigned getOpt(int* argc, char** argv, const char* keys, const char** out) {
	if (!argv || !keys || !out || !argc) {
		return GETOPT_ERR_INVALID_PARAMS;
	}
	argv += 1; // skip executable name

	const char* keyEnd = keys;
	const char* keyStart = keys;

	while (true) {
		char ch = *keyEnd;
		if (ch != ' ' && ch != '|' && ch != '\0') { // if not a delimiter char
			keyEnd++;
			continue;
		}

		size_t keyLength = keyEnd - keyStart;
		
		int code = getOptRaw(argc, argv, keyStart, keyLength, out);
		if (code != GETOPT_ERR_NOT_FOUND) { // Also return if GETOPT_ERR_INVALID_PARAMS or GETOPT_ERR_INVALID 
			return code;
		}

		keyEnd++;
		keyStart = keyEnd;

		if (*keyEnd == '\0')
			break;
	}
	return GETOPT_ERR_NOT_FOUND;
}

bool getOptName(const char* arg, char* output, size_t outputLength) {
	if (!arg || *arg != '-')
		return false;
	arg++; // skip '-'

	const char* start = arg;
	const char* end = arg;

	while (*end && *end != '=') {
		end++;
	}

	size_t nameLength = end - start + 1; // +1 for null terminator
	size_t cpyLength = (nameLength < outputLength) ? nameLength : outputLength;

	memcpy(output, start, cpyLength);
	output[cpyLength - 1] = '\0';
	return true;
}

bool hasOpt(int* argc, char** argv, const char* key) {
	const char* result = nullptr;
	return getOpt(argc, argv, key, &result) == GETOPT_SUCCESS;
}

unsigned getOptBool(int* argc, char** argv, const char* key, bool* out) {
	const char* result = nullptr;
	int code = getOpt(argc, argv, key, &result); // getOpt() to strip quotes

	if (code == GETOPT_SUCCESS) {
		if (strcmp(result, "true") == 0 || strcmp(result, "1") == 0) {
			*out = true;
			return GETOPT_SUCCESS;
		}
		if (strcmp(result, "false") == 0 || strcmp(result, "0") == 0) {
			*out = false;
			return GETOPT_SUCCESS;
		}
		return GETOPT_ERR_INVALID;
	}
	return code;
}

static bool tryParseInt(const char* src, int64_t* target) {
	if (!src)
		return false;

	int64_t sign = 1;

	if (*src == '+') {
		src++;
	} 
	else if (*src == '-') {
		sign = -1;
		src++;
	}

	int length = 0;
	const int lengthMax = 18; // floor(log10(2^63-1)) = 18 (signed i64)

	int64_t num = 0;
	while (*src) {
		char c = *src++;
		if (c < '0' || c > '9') {
			return false;
		}

		num *= 10;
		num += c - '0';

		length++;
	}

	if (length > lengthMax) {
		return false;
	}

	*target = sign * num;
	return true;
}

unsigned getOptInt64(int* argc, char** argv, const char* key, int64_t* out) {
	const char* result = nullptr;
	int code = getOpt(argc, argv, key, &result);
	
	if (code == GETOPT_SUCCESS) {
		int64_t parsed = INT64_MAX;
		if (!tryParseInt(result, &parsed)) {
			return GETOPT_ERR_INVALID;
		}
		*out = parsed;
		return GETOPT_SUCCESS;
	}
	return code;
}

unsigned getOptInt(int* argc, char** argv, const char* key, int32_t* out) {
	int64_t in;
	int code = getOptInt64(argc, argv, key, &in);
	
	if (code == GETOPT_SUCCESS) {
		if (in > INT32_MAX || in < INT32_MIN) {
			return GETOPT_ERR_INVALID;
		}
		*out = static_cast<int32_t>(in);
		return GETOPT_SUCCESS;
	}
	return code;
}
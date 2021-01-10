#include "StringPool.hpp"

#include <cstring>

namespace {
    constexpr size_t ARENA_CAPACITY = 2048;
}

StringMemoryPool::StringMemoryPool() noexcept
    : arena_start(nullptr), next_free_pos(nullptr)
{
    arena_start = nullptr;
    next_free_pos = nullptr;
}

std::string_view StringMemoryPool::store(std::string_view str) noexcept {
    const size_t length = str.size();
    const size_t arena_size = next_free_pos - arena_start;

    if (__builtin_expect(arena_start == nullptr || arena_size + length > ARENA_CAPACITY-1, 0)) { //-1 for null terminator
        push_arena();
    }

    char* __restrict dst = next_free_pos;
    std::memcpy(dst, str.data(), length);
    dst[length] = '\0';

    next_free_pos += length + 1;

    return std::string_view(dst, length);
}

void StringMemoryPool::purge_all() noexcept {
    arenas.clear();
}

void StringMemoryPool::push_arena() noexcept {
    arenas.push_back({ std::make_unique<char[]>(ARENA_CAPACITY) });
    arena_start = arenas.back().memory.get();
    next_free_pos = arena_start;
}

StringPool::StringPool() {}

const char* StringPool::get_pooled(std::string_view str) {
    if (auto it = lookup_table.find(str); it != lookup_table.end())
        return it->second;
    
    // Not found yet, add to pool
    std::string_view pooled = memory_pool.store(str);
    lookup_table.insert({ pooled, pooled.data() });

    return pooled.data();
}

void StringPool::merge_with(const StringPool& other) {

}

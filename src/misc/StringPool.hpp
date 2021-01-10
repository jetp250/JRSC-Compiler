#pragma once

#include <unordered_map>
#include <string_view>
#include <string>
#include <memory>
#include <vector>

class StringMemoryPool final {
public:
    struct Arena final {
        std::unique_ptr<char[]> memory;
    };

    StringMemoryPool() noexcept;

    std::string_view store(std::string_view str) noexcept;

    void purge_all() noexcept;

private:
    void push_arena() noexcept;

    char* arena_start;
    char* next_free_pos;
    std::vector<Arena> arenas;
};

class StringPool final
{
public:
    StringPool();

    const char* get_pooled(std::string_view str);

    void merge_with(const StringPool& other);

private:
    StringMemoryPool memory_pool;
    std::unordered_map<std::string_view, const char*> lookup_table;
};
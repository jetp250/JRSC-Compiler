
#include "Compiler.hpp"

#include "misc/SourceFile.hpp"
#include "misc/Terminal.hpp"

int main() {
    Terminal::enable_virtual_processing();

    std::optional<SourceFile> src = SourceFile::load(R"(sample_src\test5.rsc)");
    if (!src) {
        printf("Failed to load file\n");
        return 1;
    }

    return Compiler::compile(*src);
}

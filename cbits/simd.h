#include <unistd.h>
#include <stdint.h>

void avx2_memcpy(
    uint8_t *target,
    uint8_t *source,
    size_t len);

void avx2_cmpeq8(
    uint8_t byte,
    uint64_t *target,
    size_t target_length,
    uint8_t *source);

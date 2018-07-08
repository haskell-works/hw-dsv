#include <unistd.h>
#include <stdint.h>

void avx512_memcpy(
    uint8_t *target,
    uint8_t *source,
    size_t len);

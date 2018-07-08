#include "simd.h"

#include <immintrin.h>
#include <mmintrin.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>

typedef uint8_t v32si __attribute__ ((vector_size (32)));

void system_memcpy(
    char *target,
    char *source,
    size_t len) {
  memcpy(target, source, len);
}

#if defined(AVX512_ENABLED)
void avx512_memcpy(
    uint8_t *target,
    uint8_t *source,
    size_t len) {
  size_t aligned_len    = (len / 32) * 32;
  size_t remaining_len  = len - aligned_len;

  for (size_t i = 0; i < aligned_len; i += 32) {
    __m256i v = _mm256_maskz_loadu_epi8(0xffffffff, source + i);

    _mm256_mask_storeu_epi8 (target + i, 0xffffffff, v);
  }

  memcpy(target + aligned_len, source + aligned_len, remaining_len);
}

#endif

int example_main() {
  uint8_t source[32] = "01234567890123456789012345678901";
  uint8_t target[33];
  avx512_memcpy(target, source, 32);
  target[32] = 0;
  printf("%s\n", target);

  return 0;
}

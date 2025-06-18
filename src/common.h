#pragma once

#include <stdint.h>
#include <stdbool.h>
#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

typedef uint8_t   u8;
typedef int8_t    s8;
typedef uint16_t  u16;
typedef int16_t   s16;
typedef uint32_t  u32;
typedef int32_t   s32;
typedef uint64_t  u64;
typedef int64_t   s64;
typedef __uint128_t u128;
typedef __int128_t  s128;
typedef float     f32;
typedef double    f64;

#define overload __attribute__((overloadable))
#define purefunc __attribute__((pure))
#define constfunc __attribute__((const))

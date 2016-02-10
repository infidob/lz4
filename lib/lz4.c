/*
   LZ4 - Fast LZ compression algorithm
   Copyright (C) 2011-2015, Yann Collet.

   BSD 2-Clause License (http://www.opensource.org/licenses/bsd-license.php)

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are
   met:

       * Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
       * Redistributions in binary form must reproduce the above
   copyright notice, this list of conditions and the following disclaimer
   in the documentation and/or other materials provided with the
   distribution.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

   You can contact the author at :
   - LZ4 source repository : https://github.com/Cyan4973/lz4
   - LZ4 public forum : https://groups.google.com/forum/#!forum/lz4c
*/

//#define DEBUG_LEVEL 1

#ifdef DEBUG_LEVEL
static int s_debug_level = DEBUG_LEVEL;

#include <stdio.h>
#define DBGLVL(level, ...) do {if (s_debug_level >= level) fprintf(stderr, ## __VA_ARGS__); } while(0)

#else
#define DBGLVL(...) do {} while(0)

#endif

/**************************************
*  Tuning parameters
**************************************/
/*
 * HEAPMODE :
 * Select how default compression functions will allocate memory for their hash table,
 * in memory stack (0:default, fastest), or in memory heap (1:requires malloc()).
 */
#define HEAPMODE 0

/*
 * ACCELERATION_DEFAULT :
 * Select "acceleration" for LZ4_compress_fast() when parameter value <= 0
 */
#define ACCELERATION_DEFAULT 1


/**************************************
*  CPU Feature Detection
**************************************/
/* LZ4_FORCE_MEMORY_ACCESS
 * By default, access to unaligned memory is controlled by `memcpy()`, which is safe and portable.
 * Unfortunately, on some target/compiler combinations, the generated assembly is sub-optimal.
 * The below switch allow to select different access method for improved performance.
 * Method 0 (default) : use `memcpy()`. Safe and portable.
 * Method 1 : `__packed` statement. It depends on compiler extension (ie, not portable).
 *            This method is safe if your compiler supports it, and *generally* as fast or faster than `memcpy`.
 * Method 2 : direct access. This method is portable but violate C standard.
 *            It can generate buggy code on targets which generate assembly depending on alignment.
 *            But in some circumstances, it's the only known way to get the most performance (ie GCC + ARMv6)
 * See http://fastcompression.blogspot.fr/2015/08/accessing-unaligned-memory.html for details.
 * Prefer these methods in priority order (0 > 1 > 2)
 */
#ifndef LZ4_FORCE_MEMORY_ACCESS   /* can be defined externally, on command line for example */
#  if defined(__GNUC__) && ( defined(__ARM_ARCH_6__) || defined(__ARM_ARCH_6J__) || defined(__ARM_ARCH_6K__) || defined(__ARM_ARCH_6Z__) || defined(__ARM_ARCH_6ZK__) || defined(__ARM_ARCH_6T2__) )
#    define LZ4_FORCE_MEMORY_ACCESS 2
#  elif defined(__INTEL_COMPILER) || \
  (defined(__GNUC__) && ( defined(__ARM_ARCH_7__) || defined(__ARM_ARCH_7A__) || defined(__ARM_ARCH_7R__) || defined(__ARM_ARCH_7M__) || defined(__ARM_ARCH_7S__) ))
#    define LZ4_FORCE_MEMORY_ACCESS 1
#  endif
#endif

/*
 * LZ4_FORCE_SW_BITCOUNT
 * Define this parameter if your target system or compiler does not support hardware bit count
 */
#if defined(_MSC_VER) && defined(_WIN32_WCE)   /* Visual Studio for Windows CE does not support Hardware bit count */
#  define LZ4_FORCE_SW_BITCOUNT
#endif


/**************************************
*  Includes
**************************************/
#include "lz4.h"


/**************************************
*  Compiler Options
**************************************/
#ifdef _MSC_VER    /* Visual Studio */
#  define FORCE_INLINE static __forceinline
#  include <intrin.h>
#  pragma warning(disable : 4127)        /* disable: C4127: conditional expression is constant */
#  pragma warning(disable : 4293)        /* disable: C4293: too large shift (32-bits) */
#else
#  if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L)   /* C99 */
#    if defined(__GNUC__) || defined(__clang__)
#      define FORCE_INLINE static inline __attribute__((always_inline))
#    else
#      define FORCE_INLINE static inline
#    endif
#  else
#    define FORCE_INLINE static
#  endif   /* __STDC_VERSION__ */
#endif  /* _MSC_VER */

/* LZ4_GCC_VERSION is defined into lz4.h */
#if (LZ4_GCC_VERSION >= 302) || (__INTEL_COMPILER >= 800) || defined(__clang__)
#  define expect(expr,value)    (__builtin_expect ((expr),(value)) )
#else
#  define expect(expr,value)    (expr)
#endif

#define likely(expr)     expect((expr) != 0, 1)
#define unlikely(expr)   expect((expr) != 0, 0)


/**************************************
*  Memory routines
**************************************/
#include <stdlib.h>   /* malloc, calloc, free */
#define ALLOCATOR(n,s) calloc(n,s)
#define FREEMEM        free
#include <string.h>   /* memset, memcpy */
#define MEM_INIT       memset


/**************************************
*  Basic Types
**************************************/
#if defined (__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L)   /* C99 */
# include <stdint.h>
  typedef  uint8_t BYTE;
  typedef uint16_t U16;
  typedef uint32_t U32;
  typedef  int32_t S32;
  typedef uint64_t U64;
#else
  typedef unsigned char       BYTE;
  typedef unsigned short      U16;
  typedef unsigned int        U32;
  typedef   signed int        S32;
  typedef unsigned long long  U64;
#endif


/**************************************
*  Reading and writing into memory
**************************************/
#define STEPSIZE sizeof(size_t)

static unsigned LZ4_64bits(void) { return sizeof(void*)==8; }

static unsigned LZ4_isLittleEndian(void)
{
    const union { U32 i; BYTE c[4]; } one = { 1 };   // don't use static : performance detrimental
    return one.c[0];
}


#if defined(LZ4_FORCE_MEMORY_ACCESS) && (LZ4_FORCE_MEMORY_ACCESS==2)

static U16 LZ4_read16(const void* memPtr) { return *(const U16*) memPtr; }
static U32 LZ4_read32(const void* memPtr) { return *(const U32*) memPtr; }
static size_t LZ4_read_ARCH(const void* memPtr) { return *(const size_t*) memPtr; }

static void LZ4_write16(void* memPtr, U16 value) { *(U16*)memPtr = value; }

#elif defined(LZ4_FORCE_MEMORY_ACCESS) && (LZ4_FORCE_MEMORY_ACCESS==1)

/* __pack instructions are safer, but compiler specific, hence potentially problematic for some compilers */
/* currently only defined for gcc and icc */
typedef union { U16 u16; U32 u32; size_t uArch; } __attribute__((packed)) unalign;

static U16 LZ4_read16(const void* ptr) { return ((const unalign*)ptr)->u16; }
static U32 LZ4_read32(const void* ptr) { return ((const unalign*)ptr)->u32; }
static size_t LZ4_read_ARCH(const void* ptr) { return ((const unalign*)ptr)->uArch; }

static void LZ4_write16(void* memPtr, U16 value) { ((unalign*)memPtr)->u16 = value; }

#else

static U16 LZ4_read16(const void* memPtr)
{
    U16 val; memcpy(&val, memPtr, sizeof(val)); return val;
}

static U32 LZ4_read32(const void* memPtr)
{
    U32 val; memcpy(&val, memPtr, sizeof(val)); return val;
}

static size_t LZ4_read_ARCH(const void* memPtr)
{
    size_t val; memcpy(&val, memPtr, sizeof(val)); return val;
}

static void LZ4_write16(void* memPtr, U16 value)
{
    memcpy(memPtr, &value, sizeof(value));
}

#endif // LZ4_FORCE_MEMORY_ACCESS


static U16 LZ4_readLE16(const void* memPtr)
{
    if (LZ4_isLittleEndian())
    {
        return LZ4_read16(memPtr);
    }
    else
    {
        const BYTE* p = (const BYTE*)memPtr;
        return (U16)((U16)p[0] + (p[1]<<8));
    }
}

static void LZ4_writeLE16(void* memPtr, U16 value)
{
    if (LZ4_isLittleEndian())
    {
        LZ4_write16(memPtr, value);
    }
    else
    {
        BYTE* p = (BYTE*)memPtr;
        p[0] = (BYTE) value;
        p[1] = (BYTE)(value>>8);
    }
}

static void LZ4_copy8(void* dst, const void* src)
{
    memcpy(dst,src,8);
}

/* customized variant of memcpy, which can overwrite up to 7 bytes beyond dstEnd */
static void LZ4_wildCopy(void* dstPtr, const void* srcPtr, void* dstEnd)
{
    BYTE* d = (BYTE*)dstPtr;
    const BYTE* s = (const BYTE*)srcPtr;
    BYTE* const e = (BYTE*)dstEnd;

#if 0
    const size_t l2 = 8 - (((size_t)d) & (sizeof(void*)-1));
    LZ4_copy8(d,s); if (d>e-9) return;
    d+=l2; s+=l2;
#endif /* join to align */

    do { LZ4_copy8(d,s); d+=8; s+=8; } while (d<e);
}


/**************************************
*  Common Constants
**************************************/
#define MINMATCH 4

#define WILDCOPYLENGTH 8
#define LASTLITERALS 5
#define MFLIMIT (WILDCOPYLENGTH+MINMATCH)
static const int LZ4_minLength = (MFLIMIT+1);

#define KB *(1 <<10)
#define MB *(1 <<20)
#define GB *(1U<<30)

#define MAXD_LOG 16
#define MAX_DISTANCE ((1 << MAXD_LOG) - 1)

#define ML_BITS  4
#define ML_MASK  ((1U<<ML_BITS)-1)
#define RUN_BITS (8-ML_BITS)
#define RUN_MASK ((1U<<RUN_BITS)-1)


/**************************************
*  Common Utils
**************************************/
#define LZ4_STATIC_ASSERT(c)    { enum { LZ4_static_assert = 1/(int)(!!(c)) }; }   /* use only *after* variable declarations */


/**************************************
*  Common functions
**************************************/
static unsigned LZ4_NbCommonBytes (register size_t val)
{
    if (LZ4_isLittleEndian())
    {
        if (LZ4_64bits())
        {
#       if defined(_MSC_VER) && defined(_WIN64) && !defined(LZ4_FORCE_SW_BITCOUNT)
            unsigned long r = 0;
            _BitScanForward64( &r, (U64)val );
            return (int)(r>>3);
#       elif (defined(__clang__) || (LZ4_GCC_VERSION >= 304)) && !defined(LZ4_FORCE_SW_BITCOUNT)
            return (__builtin_ctzll((U64)val) >> 3);
#       else
            static const int DeBruijnBytePos[64] = { 0, 0, 0, 0, 0, 1, 1, 2, 0, 3, 1, 3, 1, 4, 2, 7, 0, 2, 3, 6, 1, 5, 3, 5, 1, 3, 4, 4, 2, 5, 6, 7, 7, 0, 1, 2, 3, 3, 4, 6, 2, 6, 5, 5, 3, 4, 5, 6, 7, 1, 2, 4, 6, 4, 4, 5, 7, 2, 6, 5, 7, 6, 7, 7 };
            return DeBruijnBytePos[((U64)((val & -(long long)val) * 0x0218A392CDABBD3FULL)) >> 58];
#       endif
        }
        else /* 32 bits */
        {
#       if defined(_MSC_VER) && !defined(LZ4_FORCE_SW_BITCOUNT)
            unsigned long r;
            _BitScanForward( &r, (U32)val );
            return (int)(r>>3);
#       elif (defined(__clang__) || (LZ4_GCC_VERSION >= 304)) && !defined(LZ4_FORCE_SW_BITCOUNT)
            return (__builtin_ctz((U32)val) >> 3);
#       else
            static const int DeBruijnBytePos[32] = { 0, 0, 3, 0, 3, 1, 3, 0, 3, 2, 2, 1, 3, 2, 0, 1, 3, 3, 1, 2, 2, 2, 2, 0, 3, 1, 2, 0, 1, 0, 1, 1 };
            return DeBruijnBytePos[((U32)((val & -(S32)val) * 0x077CB531U)) >> 27];
#       endif
        }
    }
    else   /* Big Endian CPU */
    {
        if (LZ4_64bits())
        {
#       if defined(_MSC_VER) && defined(_WIN64) && !defined(LZ4_FORCE_SW_BITCOUNT)
            unsigned long r = 0;
            _BitScanReverse64( &r, val );
            return (unsigned)(r>>3);
#       elif (defined(__clang__) || (LZ4_GCC_VERSION >= 304)) && !defined(LZ4_FORCE_SW_BITCOUNT)
            return (__builtin_clzll((U64)val) >> 3);
#       else
            unsigned r;
            if (!(val>>32)) { r=4; } else { r=0; val>>=32; }
            if (!(val>>16)) { r+=2; val>>=8; } else { val>>=24; }
            r += (!val);
            return r;
#       endif
        }
        else /* 32 bits */
        {
#       if defined(_MSC_VER) && !defined(LZ4_FORCE_SW_BITCOUNT)
            unsigned long r = 0;
            _BitScanReverse( &r, (unsigned long)val );
            return (unsigned)(r>>3);
#       elif (defined(__clang__) || (LZ4_GCC_VERSION >= 304)) && !defined(LZ4_FORCE_SW_BITCOUNT)
            return (__builtin_clz((U32)val) >> 3);
#       else
            unsigned r;
            if (!(val>>16)) { r=2; val>>=8; } else { r=0; val>>=24; }
            r += (!val);
            return r;
#       endif
        }
    }
}

static unsigned LZ4_count(const BYTE* pIn, const BYTE* pMatch, const BYTE* pInLimit)
{
    const BYTE* const pStart = pIn;

    while (likely(pIn<pInLimit-(STEPSIZE-1)))
    {
        size_t diff = LZ4_read_ARCH(pMatch) ^ LZ4_read_ARCH(pIn);
        if (!diff) { pIn+=STEPSIZE; pMatch+=STEPSIZE; continue; }
        pIn += LZ4_NbCommonBytes(diff);
        return (unsigned)(pIn - pStart);
    }

    if (LZ4_64bits()) if ((pIn<(pInLimit-3)) && (LZ4_read32(pMatch) == LZ4_read32(pIn))) { pIn+=4; pMatch+=4; }
    if ((pIn<(pInLimit-1)) && (LZ4_read16(pMatch) == LZ4_read16(pIn))) { pIn+=2; pMatch+=2; }
    if ((pIn<pInLimit) && (*pMatch == *pIn)) pIn++;
    return (unsigned)(pIn - pStart);
}


#ifndef LZ4_COMMONDEFS_ONLY
/**************************************
*  Local Constants
**************************************/
#define LZ4_HASHLOG   (LZ4_MEMORY_USAGE-2)
#define HASHTABLESIZE (1 << LZ4_MEMORY_USAGE)
#define HASH_SIZE_U32 (1 << LZ4_HASHLOG)       /* required as macro for static allocation */

static const int LZ4_64Klimit = ((64 KB) + (MFLIMIT-1));
static const U32 LZ4_skipTrigger = 6;  /* Increase this value ==> compression run slower on incompressible data */


/**************************************
*  Local Structures and types
**************************************/
typedef struct {
    U32 hashTable[HASH_SIZE_U32];
    U32 currentOffset;
    U32 initCheck;
    const BYTE* dictionary;
    BYTE* bufferStart;   /* obsolete, used for slideInputBuffer */
    U32 dictSize;
} LZ4_stream_t_internal;

typedef enum { notLimited = 0, limitedOutput = 1 } limitedOutput_directive;
typedef enum { byPtr, byU32, byU16 } tableType_t;

typedef enum { noDict = 0, withPrefix64k, usingExtDict } dict_directive;
typedef enum { noDictIssue = 0, dictSmall } dictIssue_directive;

typedef enum { endOnOutputSize = 0, endOnInputSize = 1 } endCondition_directive;
typedef enum { full = 0, partial = 1 } earlyEnd_directive;
typedef enum {
    status_decompress_ok = 0,
    /* errors */
    status_decompress_empty_output_buffer = -1,
    status_decompress_empty_input_buffer  = -2,
    status_decompress_offset_beyond_limit = -3,
    /* early end conditions */
    status_decompress_output_overflow     = -20,
    status_decompress_input_overflow      = -21,
    status_decompress_write_beyond_output = -30,
    status_decompress_read_beyond_input   = -31,
    status_decompress_output_not_consumed = -40,
    status_decompress_input_not_consumed  = -41,
    status_decompress_output_ext_match_overflow = -50,
    status_decompress_output_int_match_overflow = -51,
} status_decompress;


/**************************************
*  Local Utils
**************************************/
int LZ4_versionNumber (void) { return LZ4_VERSION_NUMBER; }
int LZ4_compressBound(int isize)  { return LZ4_COMPRESSBOUND(isize); }
int LZ4_sizeofState() { return LZ4_STREAMSIZE; }



/********************************
*  Compression functions
********************************/

static U32 LZ4_hashSequence(U32 sequence, tableType_t const tableType)
{
    if (tableType == byU16)
        return (((sequence) * 2654435761U) >> ((MINMATCH*8)-(LZ4_HASHLOG+1)));
    else
        return (((sequence) * 2654435761U) >> ((MINMATCH*8)-LZ4_HASHLOG));
}

static const U64 prime5bytes = 889523592379ULL;
static U32 LZ4_hashSequence64(size_t sequence, tableType_t const tableType)
{
    const U32 hashLog = (tableType == byU16) ? LZ4_HASHLOG+1 : LZ4_HASHLOG;
    const U32 hashMask = (1<<hashLog) - 1;
    return ((sequence * prime5bytes) >> (40 - hashLog)) & hashMask;
}

static U32 LZ4_hashSequenceT(size_t sequence, tableType_t const tableType)
{
    if (LZ4_64bits())
        return LZ4_hashSequence64(sequence, tableType);
    return LZ4_hashSequence((U32)sequence, tableType);
}

static U32 LZ4_hashPosition(const void* p, tableType_t tableType) { return LZ4_hashSequenceT(LZ4_read_ARCH(p), tableType); }

static void LZ4_putPositionOnHash(const BYTE* p, U32 h, void* tableBase, tableType_t const tableType, const BYTE* srcBase)
{
    switch (tableType)
    {
    default: /* idob: avoid warning */
    case byPtr: { const BYTE** hashTable = (const BYTE**)tableBase; hashTable[h] = p; return; }
    case byU32: { U32* hashTable = (U32*) tableBase; hashTable[h] = (U32)(p-srcBase); return; }
    case byU16: { U16* hashTable = (U16*) tableBase; hashTable[h] = (U16)(p-srcBase); return; }
    }
}

static void LZ4_putPosition(const BYTE* p, void* tableBase, tableType_t tableType, const BYTE* srcBase)
{
    U32 h = LZ4_hashPosition(p, tableType);
    DBGLVL(3, "%s h %08x\n", __FUNCTION__, h);
    LZ4_putPositionOnHash(p, h, tableBase, tableType, srcBase);
}

static const BYTE* LZ4_getPositionOnHash(U32 h, void* tableBase, tableType_t tableType, const BYTE* srcBase)
{
    if (tableType == byPtr) { const BYTE** hashTable = (const BYTE**) tableBase; return hashTable[h]; }
    if (tableType == byU32) { U32* hashTable = (U32*) tableBase; return hashTable[h] + srcBase; }
    { U16* hashTable = (U16*) tableBase; return hashTable[h] + srcBase; }   /* default, to ensure a return */
}

static const BYTE* LZ4_getPosition(const BYTE* p, void* tableBase, tableType_t tableType, const BYTE* srcBase)
{
    U32 h = LZ4_hashPosition(p, tableType);
    return LZ4_getPositionOnHash(h, tableBase, tableType, srcBase);
}

FORCE_INLINE int LZ4_compress_generic(
                 void* const ctx,
                 const char* const source,
                 char* const dest,
                 int*  const srcSizePtr,
                 const int targetDstSize,
                 const limitedOutput_directive outputLimited,
                 const tableType_t tableType,
                 const dict_directive dict,
                 const dictIssue_directive dictIssue,
                 const U32 acceleration)
{
    LZ4_stream_t_internal* const dictPtr = (LZ4_stream_t_internal*)ctx;

    const BYTE* ip = (const BYTE*) source;
    const BYTE* base;
    const BYTE* lowLimit;
    const BYTE* const lowRefLimit = ip - dictPtr->dictSize;
    const BYTE* const dictionary = dictPtr->dictionary;
    const BYTE* const dictEnd = dictionary + dictPtr->dictSize;
    const size_t dictDelta = dictEnd - (const BYTE*)source;
    const BYTE* anchor = (const BYTE*) source;
    const int inputSize = *srcSizePtr;
    const BYTE* const iend = ip + inputSize;
    const BYTE* const mflimit = iend - MFLIMIT;
    const BYTE* const matchlimit = mflimit /* limited output is stricter than previous limit of (iend - LASTLITERALS) */;

    BYTE* op = (BYTE*) dest;
    BYTE* const oend = op + targetDstSize;
    BYTE* const oMaxLit = op + targetDstSize - 2 /* offset */ - 8 /* because 8+MINMATCH==MFLIMIT */ - 1 /* token */;
    BYTE* const oMaxMatch = op + targetDstSize - (LASTLITERALS + 1 /* token */);
    BYTE* const oMaxSeq = oMaxLit - 1 /* token */;

    U32 forwardH;
    size_t refDelta=0;

    DBGLVL(1, "\t%s line: %d targetDstSize %d inputSize %d outputLimited %d dict %d dictIssue %d\n",
            __FUNCTION__, __LINE__, targetDstSize, inputSize, (int)outputLimited, (int)dict, (int)dictIssue);

    /* Init conditions */
    if ((outputLimited) && (unlikely(targetDstSize < 1))) return 0;    /* Impossible to store anything */
    if ((U32)inputSize > (U32)LZ4_MAX_INPUT_SIZE) return 0;   /* Unsupported input size, too large (or negative) */
    switch(dict)
    {
    case noDict:
    default:
        base = (const BYTE*)source;
        lowLimit = (const BYTE*)source;
        break;
    case withPrefix64k:
        base = (const BYTE*)source - dictPtr->currentOffset;
        lowLimit = (const BYTE*)source - dictPtr->dictSize;
        break;
    case usingExtDict:
        base = (const BYTE*)source - dictPtr->currentOffset;
        lowLimit = (const BYTE*)source;
        break;
    }
    if ((tableType == byU16) && (inputSize>=LZ4_64Klimit)) return 0;   /* Size too large (not within 64K limit) */
    if (inputSize<LZ4_minLength) goto _last_literals;                  /* Input too small, no compression (all literals) */

    /* First Byte */
    *srcSizePtr = 0;
    LZ4_putPosition(ip, ctx, tableType, base);
    ip++; forwardH = LZ4_hashPosition(ip, tableType);

    /* Main Loop */
    for ( ; ; )
    {
        DBGLVL(2, "Main Loop, ");

        const BYTE* match;
        BYTE* token;
        {
            const BYTE* forwardIp = ip;
            unsigned step = 1;
            unsigned searchMatchNb = acceleration << LZ4_skipTrigger;

            /* Find a match */
_find_a_match:
            do {
                U32 h = forwardH;
                ip = forwardIp;
                forwardIp += step;
                step = (searchMatchNb++ >> LZ4_skipTrigger);

                if (unlikely(forwardIp > mflimit)) {
                    DBGLVL(2, "\n\tline: %d Ioffset %d forward-ofs %d sMNb %u step %u \n",
                            __LINE__, (int)((const char *)ip-source), (int)(forwardIp-ip), searchMatchNb, step);
                    goto _last_literals;
                }

                match = LZ4_getPositionOnHash(h, ctx, tableType, base);

                DBGLVL(3, "Find a match, ip %d forwardIp %d step %d h %08x base %p\n",
                        (int) (((const char*)ip)-source), (int)(forwardIp-ip), step, h, base);

                if (dict==usingExtDict)
                {
                    if (match<(const BYTE*)source)
                    {
                        refDelta = dictDelta;
                        lowLimit = dictionary;
                    }
                    else
                    {
                        refDelta = 0;
                        lowLimit = (const BYTE*)source;
                    }
                }
                forwardH = LZ4_hashPosition(forwardIp, tableType);
                LZ4_putPositionOnHash(ip, h, ctx, tableType, base);

                DBGLVL(3, "update hash match %d lowRefLimit %d dictIssue %d tableType %d refDelta %zd\n",
                        (int) (((const char*)(match+refDelta))-source),
                        (int) (((const char*)lowRefLimit)-source), dictIssue, tableType, refDelta);

            } while ( ((dictIssue==dictSmall) ? (match < lowRefLimit) : 0)
                || ((tableType==byU16) ? 0 : (match + MAX_DISTANCE < ip))
                || (LZ4_read32(match+refDelta) != LZ4_read32(ip)) );
            /* if out of scope due to window overflow - skip and find another match */
            if ((dict != noDict) && unlikely(match >= ip)) goto _find_a_match;
        }

        /* Catch up */
        while ((ip>anchor) && (match+refDelta > lowLimit) && (unlikely(ip[-1]==match[refDelta-1]))) { ip--; match--; }

        {
            /* Encode Literal length */
            unsigned litLength = (unsigned)(ip - anchor);

            DBGLVL(2, "litLength %d, ", litLength);

            token = op++;
            if ((outputLimited) && (unlikely(op + ((litLength+240)/255) + litLength > oMaxLit)))
            {
                DBGLVL(2, "\n\tline: %d Ooffset %d litLength %u \n",
                        __LINE__, (int)((const char *)op-dest), litLength);
                /* Not enough space for a last match */
                op--;
                goto _last_literals;
            }
            if (litLength>=RUN_MASK)
            {
                int len = (int)litLength-RUN_MASK;
                *token=(RUN_MASK<<ML_BITS);
                for(; len >= 255 ; len-=255) *op++ = 255;
                *op++ = (BYTE)len;
            }
            else *token = (BYTE)(litLength<<ML_BITS);

            /* Copy Literals */
            LZ4_wildCopy(op, anchor, op+litLength);
            op+=litLength;
        }

_next_match:
        /* Encode Offset */
        DBGLVL(2, "Offset %d, ", (int)(ip-match));

        LZ4_writeLE16(op, (U16)(ip-match)); op+=2;

        DBGLVL(2, "\nMatchLength %d %d\n", (int) (((const char*)ip)-source), (int) (((const char*)op)-dest));
        /* Encode MatchLength */
        {
            size_t matchLength;

            if ((dict==usingExtDict) && (lowLimit==dictionary))
            {
                const BYTE* limit;
                match += refDelta;
                limit = ip + (dictEnd-match);
                if (limit > matchlimit) limit = matchlimit;
                matchLength = LZ4_count(ip+MINMATCH, match+MINMATCH, limit);
                DBGLVL(2, "ExtDict matchLength %zd, ", matchLength);

                if ((outputLimited) && (unlikely(op + ((matchLength+240)/255) > oMaxMatch)))
                {
                    /* Match description too long : reduce it */
                    matchLength = (15-1) + (oMaxMatch-op) * 255;
                    /* Avoid searching further when output limit is reached */
                    limit = NULL;
                }

                ip += MINMATCH + matchLength;
                if (ip==limit)
                {
                    unsigned more = LZ4_count(ip, (const BYTE*)source, matchlimit);
                    matchLength += more;
                    ip += more;
                }
            }
            else
            {
                matchLength = LZ4_count(ip+MINMATCH, match+MINMATCH, matchlimit);
                DBGLVL(2, "matchLength %zd, ", matchLength);

                if ((outputLimited) && (unlikely(op + ((matchLength+240)/255) > oMaxMatch)))
                {
                    /* Match description too long : reduce it */
                    matchLength = (15-1) + (oMaxMatch-op) * 255;
                }

                ip += MINMATCH + matchLength;
            }

            if (matchLength>=ML_MASK)
            {
                *token += ML_MASK;
                matchLength -= ML_MASK;
                for (; matchLength >= 510 ; matchLength-=510) { *op++ = 255; *op++ = 255; }
                if (matchLength >= 255) { matchLength-=255; *op++ = 255; }
                *op++ = (BYTE)matchLength;
            }
            else *token += (BYTE)(matchLength);
        }

        anchor = ip;

        /* Test end of chunk */
        if (ip > mflimit) break;
        if ((outputLimited) && (unlikely(op > oMaxSeq))) break;

        DBGLVL(2, "EOL\n");
        /* Fill table */
        LZ4_putPosition(ip-2, ctx, tableType, base);

        /* Test next position */
        match = LZ4_getPosition(ip, ctx, tableType, base);
        if (dict==usingExtDict)
        {
            if (match<(const BYTE*)source)
            {
                refDelta = dictDelta;
                lowLimit = dictionary;
            }
            else
            {
                refDelta = 0;
                lowLimit = (const BYTE*)source;
            }
        }

        LZ4_putPosition(ip, ctx, tableType, base);
        if ( ((dictIssue==dictSmall) ? (match>=lowRefLimit) : 1)
            && (match+MAX_DISTANCE>=ip)
            && (LZ4_read32(match+refDelta)==LZ4_read32(ip))
            && ((dict != noDict) ? (match < ip) : 1))
        { token=op++; *token=0; goto _next_match; }

        /* Prepare next loop */
        forwardH = LZ4_hashPosition(++ip, tableType);
    }

_last_literals:
    /* Encode Last Literals */
    {
        size_t lastRun = (size_t)(iend - anchor);
        DBGLVL(2, "\nEncode Last Literals (lastRun %i)\n", (int)lastRun);

        if ((outputLimited) && (unlikely(op + 1 /* token */ + ((lastRun+240)/255) /* litLength */ + lastRun /* literals */ > oend)))
        {
            /* adapt lastRun to fill 'dst' */
            lastRun  = (oend-op) - 1;
            lastRun -= (lastRun+240)/255;
        }
        ip = anchor + lastRun;

        if (lastRun >= RUN_MASK)
        {
            size_t accumulator = lastRun - RUN_MASK;
            *op++ = RUN_MASK << ML_BITS;
            for(; accumulator >= 255 ; accumulator-=255) *op++ = 255;
            *op++ = (BYTE) accumulator;
        }
        else
        {
            *op++ = (BYTE)(lastRun<<ML_BITS);
        }
        memcpy(op, anchor, lastRun);
        op += lastRun;
    }

    /* End */
    *srcSizePtr = (int) (((const char*)ip)-source);

    DBGLVL(1, "\t%s line: %d inputSize %d outputSize %d\n",
            __FUNCTION__, __LINE__, inputSize, (int) (((char*)op)-dest));

    return (int) (((char*)op)-dest);
}

int LZ4_compress_fast_extState(void* state, const char* source, char* dest, int srcSize, int maxOutputSize, int acceleration)
{
    DBGLVL(1, "%s srcSize %d maxOutputSize %d\n", __FUNCTION__, srcSize, maxOutputSize);
    int tempSrcSize = srcSize;
    int result = LZ4_compress_fast_destSize_extState(state, source, dest, &tempSrcSize, maxOutputSize, acceleration);
    return (tempSrcSize == srcSize) ? result : 0;
}
int LZ4_compress_fast_destSize_extState(void* state, const char* source, char* dest, int* srcSizePtr, int maxOutputSize, int acceleration)
{
    DBGLVL(1, "%s *srcSizePtr %d maxOutputSize %d\n", __FUNCTION__, *srcSizePtr, maxOutputSize);
    LZ4_resetStream((LZ4_stream_t*)state);
    if (acceleration < 1) acceleration = ACCELERATION_DEFAULT;

    int inputSize = *srcSizePtr;
    if (maxOutputSize >= LZ4_compressBound(inputSize))
    {
        if (inputSize < LZ4_64Klimit)
            return LZ4_compress_generic(state, source, dest, srcSizePtr, 0, notLimited, byU16,                        noDict, noDictIssue, acceleration);
        else
            return LZ4_compress_generic(state, source, dest, srcSizePtr, 0, notLimited, LZ4_64bits() ? byU32 : byPtr, noDict, noDictIssue, acceleration);
    }
    else
    {
        if (inputSize < LZ4_64Klimit)
            return LZ4_compress_generic(state, source, dest, srcSizePtr, maxOutputSize, limitedOutput, byU16,                        noDict, noDictIssue, acceleration);
        else
            return LZ4_compress_generic(state, source, dest, srcSizePtr, maxOutputSize, limitedOutput, LZ4_64bits() ? byU32 : byPtr, noDict, noDictIssue, acceleration);
    }
}


int LZ4_compress_fast(const char* source, char* dest, int inputSize, int maxOutputSize, int acceleration)
{
#if (HEAPMODE)
    void* ctxPtr = ALLOCATOR(1, sizeof(LZ4_stream_t));   /* malloc-calloc always properly aligned */
#else
    LZ4_stream_t ctx;
    void* ctxPtr = &ctx;
#endif

    int result = LZ4_compress_fast_extState(ctxPtr, source, dest, inputSize, maxOutputSize, acceleration);

#if (HEAPMODE)
    FREEMEM(ctxPtr);
#endif
    return result;
}


int LZ4_compress_default(const char* source, char* dest, int inputSize, int maxOutputSize)
{
    return LZ4_compress_fast(source, dest, inputSize, maxOutputSize, 1);
}


/* hidden debug function */
/* strangely enough, gcc generates faster code when this function is uncommented, even if unused */
int LZ4_compress_fast_force(const char* source, char* dest, int inputSize, int maxOutputSize, int acceleration)
{
    LZ4_stream_t ctx;

    LZ4_resetStream(&ctx);

    int tempInputSize = inputSize;
    int result;
    if (inputSize < LZ4_64Klimit)
        result = LZ4_compress_generic(&ctx, source, dest, &tempInputSize, maxOutputSize, limitedOutput, byU16,                        noDict, noDictIssue, acceleration);
    else
        result = LZ4_compress_generic(&ctx, source, dest, &tempInputSize, maxOutputSize, limitedOutput, LZ4_64bits() ? byU32 : byPtr, noDict, noDictIssue, acceleration);
    return (tempInputSize == inputSize) ? result : 0;
}



int LZ4_compress_destSize(const char* src, char* dst, int* srcSizePtr, int targetDstSize)
{
#if (HEAPMODE)
    void* ctx = ALLOCATOR(1, sizeof(LZ4_stream_t));   /* malloc-calloc always properly aligned */
#else
    LZ4_stream_t ctxBody;
    void* ctx = &ctxBody;
#endif

    int result = LZ4_compress_fast_destSize_extState(ctx, src, dst, srcSizePtr, targetDstSize, ACCELERATION_DEFAULT);

#if (HEAPMODE)
    FREEMEM(ctx);
#endif
    return result;
}



/********************************
*  Streaming functions
********************************/

LZ4_stream_t* LZ4_createStream(void)
{
    LZ4_stream_t* lz4s = (LZ4_stream_t*)ALLOCATOR(8, LZ4_STREAMSIZE_U64);
    LZ4_STATIC_ASSERT(LZ4_STREAMSIZE >= sizeof(LZ4_stream_t_internal));    /* A compilation error here means LZ4_STREAMSIZE is not large enough */
    LZ4_resetStream(lz4s);
    return lz4s;
}

void LZ4_resetStream (LZ4_stream_t* LZ4_stream)
{
    MEM_INIT(LZ4_stream, 0, sizeof(LZ4_stream_t));
}

int LZ4_freeStream (LZ4_stream_t* LZ4_stream)
{
    FREEMEM(LZ4_stream);
    return (0);
}


#define HASH_UNIT sizeof(size_t)
int LZ4_loadDict (LZ4_stream_t* LZ4_dict, const char* dictionary, int dictSize)
{
    LZ4_stream_t_internal* dict = (LZ4_stream_t_internal*) LZ4_dict;
    const BYTE* p = (const BYTE*)dictionary;
    const BYTE* const dictEnd = p + dictSize;
    const BYTE* base;

    if ((dict->initCheck) || (dict->currentOffset > 1 GB))  /* Uninitialized structure, or reuse overflow */
        LZ4_resetStream(LZ4_dict);

    if (dictSize < (int)HASH_UNIT)
    {
        dict->dictionary = NULL;
        dict->dictSize = 0;
        return 0;
    }

    if ((dictEnd - p) > 64 KB) p = dictEnd - 64 KB;
    dict->currentOffset += 64 KB;
    base = p - dict->currentOffset;
    dict->dictionary = p;
    dict->dictSize = (U32)(dictEnd - p);
    dict->currentOffset += dict->dictSize;

    while (p <= dictEnd-HASH_UNIT)
    {
        LZ4_putPosition(p, dict->hashTable, byU32, base);
        p+=3;
    }

    return dict->dictSize;
}


static void LZ4_renormDictT(LZ4_stream_t_internal* LZ4_dict, const BYTE* src)
{
    if ((LZ4_dict->currentOffset > 0x80000000) ||
        ((size_t)LZ4_dict->currentOffset > (size_t)src))   /* address space overflow */
    {
        DBGLVL(1, "\n   rescale hash table   \n");
        /* rescale hash table */
        U32 delta = LZ4_dict->currentOffset - 64 KB;
        const BYTE* dictEnd = LZ4_dict->dictionary + LZ4_dict->dictSize;
        int i;
        for (i=0; i<HASH_SIZE_U32; i++)
        {
            if (LZ4_dict->hashTable[i] < delta) LZ4_dict->hashTable[i]=0;
            else LZ4_dict->hashTable[i] -= delta;
        }
        LZ4_dict->currentOffset = 64 KB;
        if (LZ4_dict->dictSize > 64 KB) LZ4_dict->dictSize = 64 KB;
        LZ4_dict->dictionary = dictEnd - LZ4_dict->dictSize;
    }
}


int LZ4_compress_fast_continue (LZ4_stream_t* LZ4_stream, const char* source, char* dest, int srcSize, int maxOutputSize, int acceleration)
{
    int tempSrcSize = srcSize;
    int result = LZ4_compress_fast_destSize_continue(LZ4_stream, source, dest, &tempSrcSize, maxOutputSize, acceleration);

    int diff = srcSize - tempSrcSize;
    if (diff) {
        LZ4_stream_t_internal *lz4s_ctx = (LZ4_stream_t_internal *)LZ4_stream;
        lz4s_ctx->dictSize      += diff;
        lz4s_ctx->currentOffset += diff;
    }

    return (tempSrcSize == srcSize) ? result : 0;
}
int LZ4_compress_fast_destSize_continue (LZ4_stream_t* LZ4_stream, const char* source, char* dest, int *srcSizePtr, int maxOutputSize, int acceleration)
{
    LZ4_stream_t_internal* streamPtr = (LZ4_stream_t_internal*)LZ4_stream;
    const BYTE* const dictEnd = streamPtr->dictionary + streamPtr->dictSize;

    const BYTE* smallest = (const BYTE*) source;
    if (streamPtr->initCheck) return 0;   /* Uninitialized structure detected */
    if ((streamPtr->dictSize>0) && (smallest>dictEnd)) smallest = dictEnd;
    LZ4_renormDictT(streamPtr, smallest);
    if (acceleration < 1) acceleration = ACCELERATION_DEFAULT;
    int inputSize = *srcSizePtr;

    /* Check overlapping input/dictionary space */
    {
        const BYTE* sourceEnd = (const BYTE*) source + inputSize;
        DBGLVL(2, "\n source %p sourceEnd %p dictionary %p dictEnd %p", source, sourceEnd, streamPtr->dictionary, dictEnd);
        if ((sourceEnd > streamPtr->dictionary) && (sourceEnd < dictEnd))
        {
            DBGLVL(1, "\n overlapping inputSize %d dictSize %d\n", inputSize, streamPtr->dictSize);
            streamPtr->dictSize = (U32)(dictEnd - sourceEnd);
            if (streamPtr->dictSize > 64 KB) streamPtr->dictSize = 64 KB;
            if (streamPtr->dictSize < 4) streamPtr->dictSize = 0;
            streamPtr->dictionary = dictEnd - streamPtr->dictSize;
        }
    }

    /* prefix mode : source data follows dictionary */
    if (dictEnd == (const BYTE*)source)
    {
        DBGLVL(1, "\nprefix currentOffset %d dictSize %d source %p dest %p inputSize %d maxOutputSize %d\n", streamPtr->currentOffset, streamPtr->dictSize, source, dest, inputSize, maxOutputSize);
        int result;
        if ((streamPtr->dictSize < 64 KB) && (streamPtr->dictSize < streamPtr->currentOffset))
            result = LZ4_compress_generic(LZ4_stream, source, dest, srcSizePtr, maxOutputSize, limitedOutput, byU32, withPrefix64k, dictSmall, acceleration);
        else
            result = LZ4_compress_generic(LZ4_stream, source, dest, srcSizePtr, maxOutputSize, limitedOutput, byU32, withPrefix64k, noDictIssue, acceleration);
        inputSize = *srcSizePtr;
        streamPtr->dictSize += (U32)inputSize;
        streamPtr->currentOffset += (U32)inputSize;
        return result;
    }

    /* external dictionary mode */
    {
        DBGLVL(1, "\nexternal currentOffset %d dictSize %d source %p dest %p\n", streamPtr->currentOffset, streamPtr->dictSize, source, dest);
        int result;
        if ((streamPtr->dictSize < 64 KB) && (streamPtr->dictSize < streamPtr->currentOffset))
            result = LZ4_compress_generic(LZ4_stream, source, dest, srcSizePtr, maxOutputSize, limitedOutput, byU32, usingExtDict, dictSmall, acceleration);
        else
            result = LZ4_compress_generic(LZ4_stream, source, dest, srcSizePtr, maxOutputSize, limitedOutput, byU32, usingExtDict, noDictIssue, acceleration);
        inputSize = *srcSizePtr;
        streamPtr->dictionary = (const BYTE*)source;
        streamPtr->dictSize = (U32)inputSize;
        streamPtr->currentOffset += (U32)inputSize;
        return result;
    }
}


/* Hidden debug function, to force external dictionary mode */
int LZ4_compress_forceExtDict (LZ4_stream_t* LZ4_dict, const char* source, char* dest, int inputSize)
{
    LZ4_stream_t_internal* streamPtr = (LZ4_stream_t_internal*)LZ4_dict;
    int result;
    const BYTE* const dictEnd = streamPtr->dictionary + streamPtr->dictSize;

    const BYTE* smallest = dictEnd;
    if (smallest > (const BYTE*) source) smallest = (const BYTE*) source;
    LZ4_renormDictT((LZ4_stream_t_internal*)LZ4_dict, smallest);

    int tempInputSize = inputSize;
    result = LZ4_compress_generic(LZ4_dict, source, dest, &tempInputSize, 0, notLimited, byU32, usingExtDict, noDictIssue, 1);
    if (tempInputSize != inputSize) result = 0;

    streamPtr->dictionary = (const BYTE*)source;
    streamPtr->dictSize = (U32)inputSize;
    streamPtr->currentOffset += (U32)inputSize;

    return result;
}


int LZ4_saveDict (LZ4_stream_t* LZ4_dict, char* safeBuffer, int dictSize)
{
    LZ4_stream_t_internal* dict = (LZ4_stream_t_internal*) LZ4_dict;
    const BYTE* previousDictEnd = dict->dictionary + dict->dictSize;

    if ((U32)dictSize > 64 KB) dictSize = 64 KB;   /* useless to define a dictionary > 64 KB */
    if ((U32)dictSize > dict->dictSize) dictSize = dict->dictSize;

    memmove(safeBuffer, previousDictEnd - dictSize, dictSize);

    dict->dictionary = (const BYTE*)safeBuffer;
    dict->dictSize = (U32)dictSize;

    return dictSize;
}



/*******************************
*  Decompression functions
*******************************/
/*
 * This generic decompression function cover all use cases.
 * It shall be instantiated several times, using different sets of directives
 * Note that it is essential this generic function is really inlined,
 * in order to remove useless branches during compilation optimization.
 */
FORCE_INLINE status_decompress LZ4_decompress_destSize_generic(
                 const char* const source,
                 char* const dest,

                 int* inputSizePtr,
                 int* outputSizePtr,         /* If endOnInput==endOnInputSize, this value is the max size of Output Buffer. */

                 int endInputSize,         /* stop when  input has been consumed */
                 int endOutputSize,        /* stop when output has been consumed */

                 int dict,               /* noDict, withPrefix64k, usingExtDict */
                 const BYTE* const lowPrefix,  /* == dest if dict == noDict */
                 const BYTE* const dictStart,  /* only if dict==usingExtDict */
                 const size_t dictSize         /* note : = 0 if noDict */
                 )
{
    DBGLVL(1, "%s: endInputSize %d endOutputSize %d dict %d dictSize %zd\n",
            __FUNCTION__, endInputSize, endOutputSize, dict, dictSize);
    /* Local Variables */
    status_decompress rc = status_decompress_ok;
    int inputSize  = *inputSizePtr;
    int outputSize = *outputSizePtr;
    *inputSizePtr  = 0;
    *outputSizePtr = 0;

    const BYTE* ip = (const BYTE*) source;
    const BYTE* const iend = ip + inputSize;

    BYTE* op = (BYTE*) dest;
    BYTE* const oend = op + outputSize;
    BYTE* cpy;
    BYTE* oexit = op + endOutputSize; /* only used if partialDecoding */
    const BYTE* const lowLimit = lowPrefix - dictSize;
    const BYTE* token_ip;
    const BYTE* token_op;

    const BYTE* const dictEnd = (const BYTE*)dictStart + dictSize;
    const unsigned dec32table[] = {4, 1, 2,  1, 4, 4, 4, 4};
    const int      dec64table[] = {0, 0, 0, -1, 0, 1, 2, 3};

    const int partialDecoding = (endOutputSize  < outputSize);
    const int safeDecode      = (            0 !=  inputSize);
    const int endOnInput      = safeDecode;
    const int checkOffset = ((safeDecode) && (dictSize < (int)(64 KB)));


    /* Special cases */
    if ((partialDecoding) && (oexit> oend-MFLIMIT)) oexit = oend-MFLIMIT;                         /* targetOutputSize too high => decode everything */
    if ((endInputSize) && (inputSize==0)) return status_decompress_empty_input_buffer;
    if ( (endOnInput) && (unlikely(outputSize==0))) return ((inputSize==1) && (*ip==0))    ? status_decompress_ok : status_decompress_empty_output_buffer;      /* Empty output buffer */
    if ((!endOnInput) && (unlikely(outputSize==0))) return ((*ip==0) && (*inputSizePtr=1)) ? status_decompress_ok : status_decompress_empty_output_buffer;      /* Empty output buffer */

    DBGLVL(1, "%s: partialDecoding %d safeDecode %d endOnInput %d checkOffset %d\n",
            __FUNCTION__, partialDecoding, safeDecode, endOnInput, checkOffset);

    /* Main Loop */
    while (1)
    {
        unsigned token;
        size_t length;
        const BYTE* match;
        size_t offset;

        /* mark token position in input (and matching output position) in case of early-end */
        token_ip = ip;
        token_op = op;

        /* get literal length */
        token = *ip++;
        if ((length=(token>>ML_BITS)) == RUN_MASK)
        {
            unsigned s;
            do
            {
                s = *ip++;
                length += s;
            }
            while ( likely(endOnInput ? ip<iend-RUN_MASK : 1) && (s==255) );
            /* overflow detection */
            if ((safeDecode) && unlikely((size_t)(op+length)<(size_t)(op))) { rc = status_decompress_output_overflow; goto _early_end;}
            if ((safeDecode) && unlikely((size_t)(ip+length)<(size_t)(ip))) { rc = status_decompress_input_overflow ; goto _early_end;}
        }

        /* copy literals */
        DBGLVL(2, "copy literals length %6zd, ", length);
        cpy = op+length;
        if (((endOnInput) && ((cpy>(partialDecoding?oexit:oend-MFLIMIT)) || (ip+length>iend-(2+1+LASTLITERALS))) )
            || ((!endOnInput) && (cpy>oend-WILDCOPYLENGTH)))
        {
            if (partialDecoding)
            {
                /* Check: write attempt beyond end of output buffer */
                if (cpy > oend) {rc = status_decompress_write_beyond_output; goto _early_end;}
                /* Check: read attempt beyond end of input buffer */
                if ((endOnInput) && (ip+length > iend)) {rc = status_decompress_read_beyond_input; goto _early_end;}
            }
            else
            {
                /* Check: block decoding must stop exactly there */
                if ((!endOnInput) && (cpy != oend)) {rc = status_decompress_output_not_consumed; DBGLVL(1, "copy literals length %zd oend-cpy %li\n", length, oend-cpy); goto _early_end;}
                /* Check: input must be consumed */
                if ((endOnInput) && ((ip+length != iend) || (cpy > oend))) {rc = status_decompress_input_not_consumed; goto _early_end;}
            }
            memcpy(op, ip, length);
            ip += length;
            op += length;
            DBGLVL(2, "copy last literals %zd\n", length);
            break;     /* Necessarily EOF, due to parsing restrictions */
        }
        LZ4_wildCopy(op, ip, cpy);
        ip += length; op = cpy;

        /* get offset */
        offset = LZ4_readLE16(ip); ip+=2;
        DBGLVL(2, "get offset %6zd, ", offset);
        match = op - offset;
        /* Check: offset outside buffers */
        if ((checkOffset) && (unlikely(match < lowLimit))) {rc = status_decompress_offset_beyond_limit; goto _output_error;}

        /* get matchlength */
        length = token & ML_MASK;
        if (length == ML_MASK)
        {
            unsigned s;
            do
            {
                /* overflow detection */
                if ((endOnInput) && (ip > iend-LASTLITERALS)) {rc = status_decompress_input_overflow; goto _early_end;}
                s = *ip++;
                length += s;
            } while (s==255);
            /* overflow detection */
            if ((safeDecode) && unlikely((size_t)(op+length)<(size_t)op)) {rc = status_decompress_output_overflow; goto _early_end;}
        }
        length += MINMATCH;
        DBGLVL(2, "get matchlength %6zd, ", length);

        /* check external dictionary */
        if ((dict==usingExtDict) && (match < lowPrefix))
        {
            /* Check: respect parsing restriction */
            if (unlikely(op+length > oend-LASTLITERALS)) {rc = status_decompress_output_ext_match_overflow; goto _early_end;}

            if (length <= (size_t)(lowPrefix-match))
            {
                /* match can be copied as a single segment from external dictionary */
                match = dictEnd - (lowPrefix-match);
                memmove(op, match, length); op += length;
            }
            else
            {
                /* match encompass external dictionary and current block */
                size_t copySize = (size_t)(lowPrefix-match);
                memcpy(op, dictEnd - copySize, copySize);
                op += copySize;
                copySize = length - copySize;
                if (copySize > (size_t)(op-lowPrefix))   /* overlap copy */
                {
                    BYTE* const endOfMatch = op + copySize;
                    const BYTE* copyFrom = lowPrefix;
                    while (op < endOfMatch) *op++ = *copyFrom++;
                }
                else
                {
                    memcpy(op, lowPrefix, copySize);
                    op += copySize;
                }
            }
            continue;
        }

        /* copy match within block */
        DBGLVL(2, "copy match within block %zd\n", length);
        cpy = op + length;
        if (unlikely(offset<8))
        {
            const int dec64 = dec64table[offset];
            op[0] = match[0];
            op[1] = match[1];
            op[2] = match[2];
            op[3] = match[3];
            match += dec32table[offset];
            memcpy(op+4, match, 4);
            match -= dec64;
        } else { LZ4_copy8(op, match); match+=8; }
        op += 8;

        if (unlikely(cpy>oend-12))
        {
            BYTE* const oCopyLimit = oend-(WILDCOPYLENGTH-1);
            /* Check: last LASTLITERALS bytes must be literals (uncompressed) */
            if (cpy > oend-LASTLITERALS) {rc = status_decompress_output_int_match_overflow; DBGLVL(1, "token 0x%02x copy match offset %zd length %zd oend-cpy %li\n", token, offset, length, oend-cpy); goto _early_end;}

            if (op < oCopyLimit)
            {
                LZ4_wildCopy(op, match, oCopyLimit);
                match += oCopyLimit - op;
                op = oCopyLimit;
            }
            while (op<cpy) *op++ = *match++;
        }
        else
            LZ4_wildCopy(op, match, cpy);
        op=cpy;   /* correction */
    }

    /* end of decoding (OK or unrecoverable error) */
//_decode_end:
_output_error:
    *inputSizePtr  = (int) (((const char*)ip)-source);
    *outputSizePtr = (int) (((char*)op)-dest);
    return rc;

    /*
     * Another call with more input data and/or more output space may resolve this
     * (e.g., if data is split to blocks), as long as we begin at current token and
     * buffers are large enough on the next call.
     */
_early_end:
    *inputSizePtr  = (int) (((const char*)token_ip)-source);
    *outputSizePtr = (int) (((const char*)token_op)-dest);
    return rc;
}

static int LZ4_decompress_generic(
                 const char* const source,
                 char* const dest,
                 int inputSize,
                 int outputSize,
                 int endOnInput,
                 int partialDecoding,
                 int targetOutputSize,
                 int dict,               /* noDict, withPrefix64k, usingExtDict */
                 const BYTE* const lowPrefix,  /* == dest if dict == noDict */
                 const BYTE* const dictStart,  /* only if dict==usingExtDict */
                 const size_t dictSize         /* note : = 0 if noDict */
        )
{
    DBGLVL(1, "%s: inputSize %d outputSize %d endOnInput %d partialDecoding %d targetOutputSize %d dict %d dictSize %zd dest %p lowPrefix %p dictStart %p\n",
            __FUNCTION__, inputSize, outputSize, endOnInput, partialDecoding, targetOutputSize, dict, dictSize, dest, lowPrefix, dictStart);

    int endInputSize = endOnInput;
    int endOutputSize = (partialDecoding) ? targetOutputSize : outputSize;
    int iSize = inputSize;
    int oSize = outputSize;
    const status_decompress result = LZ4_decompress_destSize_generic(
            source, dest, &iSize, &oSize, endInputSize, endOutputSize, dict, lowPrefix, dictStart, dictSize);

    DBGLVL(1, "%s: result %d iSize %d oSize %d\n",
            __FUNCTION__, result, iSize, oSize);

    if (result == status_decompress_ok) {
        if (endOnInput)
            return oSize; /* Nb of output bytes decoded */
        else
            return iSize; /* Nb of input bytes read */
    }

    return -iSize - 1;
}


int LZ4_decompress_safe(const char* source, char* dest, int compressedSize, int maxDecompressedSize)
{
    DBGLVL(1, "%s: compressedSize %d maxDecompressedSize %d\n",
            __FUNCTION__, compressedSize, maxDecompressedSize);
    return LZ4_decompress_generic(source, dest, compressedSize, maxDecompressedSize, endOnInputSize, full, 0, noDict, (BYTE*)dest, NULL, 0);
}

int LZ4_decompress_safe_partial(const char* source, char* dest, int compressedSize, int targetOutputSize, int maxDecompressedSize)
{
    DBGLVL(1, "%s: compressedSize %d targetOutputSize %d maxDecompressedSize %d\n",
            __FUNCTION__, compressedSize, targetOutputSize, maxDecompressedSize);
    return LZ4_decompress_generic(source, dest, compressedSize, maxDecompressedSize, endOnInputSize, partial, targetOutputSize, noDict, (BYTE*)dest, NULL, 0);
}

int LZ4_decompress_fast(const char* source, char* dest, int originalSize)
{
    DBGLVL(1, "%s: originalSize %d\n",
            __FUNCTION__, originalSize);
    return LZ4_decompress_generic(source, dest, 0, originalSize, endOnOutputSize, full, 0, withPrefix64k, (BYTE*)(dest - 64 KB), NULL, 64 KB);
}


/* streaming decompression functions */

typedef struct
{
    const BYTE* externalDict;
    size_t extDictSize;
    const BYTE* prefixEnd;
    size_t prefixSize;
} LZ4_streamDecode_t_internal;

/*
 * If you prefer dynamic allocation methods,
 * LZ4_createStreamDecode()
 * provides a pointer (void*) towards an initialized LZ4_streamDecode_t structure.
 */
LZ4_streamDecode_t* LZ4_createStreamDecode(void)
{
    LZ4_streamDecode_t* lz4s = (LZ4_streamDecode_t*) ALLOCATOR(1, sizeof(LZ4_streamDecode_t));
    return lz4s;
}

int LZ4_freeStreamDecode (LZ4_streamDecode_t* LZ4_stream)
{
    FREEMEM(LZ4_stream);
    return 0;
}

/*
 * LZ4_setStreamDecode
 * Use this function to instruct where to find the dictionary
 * This function is not necessary if previous data is still available where it was decoded.
 * Loading a size of 0 is allowed (same effect as no dictionary).
 * Return : 1 if OK, 0 if error
 */
int LZ4_setStreamDecode (LZ4_streamDecode_t* LZ4_streamDecode, const char* dictionary, int dictSize)
{
    LZ4_streamDecode_t_internal* lz4sd = (LZ4_streamDecode_t_internal*) LZ4_streamDecode;
    lz4sd->prefixSize = (size_t) dictSize;
    lz4sd->prefixEnd = (const BYTE*) dictionary + dictSize;
    lz4sd->externalDict = NULL;
    lz4sd->extDictSize  = 0;
    return 1;
}

/*
*_continue() :
    These decoding functions allow decompression of multiple blocks in "streaming" mode.
    Previously decoded blocks must still be available at the memory position where they were decoded.
    If it's not possible, save the relevant part of decoded data into a safe buffer,
    and indicate where it stands using LZ4_setStreamDecode()
*/
int LZ4_decompress_safe_continue (LZ4_streamDecode_t* LZ4_streamDecode, const char* source, char* dest, int compressedSize, int maxOutputSize)
{
    LZ4_streamDecode_t_internal* lz4sd = (LZ4_streamDecode_t_internal*) LZ4_streamDecode;
    int result;

    if (lz4sd->prefixEnd == (BYTE*)dest)
    {
        result = LZ4_decompress_generic(source, dest, compressedSize, maxOutputSize,
                                        endOnInputSize, full, 0,
                                        usingExtDict, lz4sd->prefixEnd - lz4sd->prefixSize, lz4sd->externalDict, lz4sd->extDictSize);
        if (result <= 0) return result;
        lz4sd->prefixSize += result;
        lz4sd->prefixEnd  += result;
    }
    else
    {
        lz4sd->extDictSize = lz4sd->prefixSize;
        lz4sd->externalDict = lz4sd->prefixEnd - lz4sd->extDictSize;
        result = LZ4_decompress_generic(source, dest, compressedSize, maxOutputSize,
                                        endOnInputSize, full, 0,
                                        usingExtDict, (BYTE*)dest, lz4sd->externalDict, lz4sd->extDictSize);
        if (result <= 0) return result;
        lz4sd->prefixSize = result;
        lz4sd->prefixEnd  = (BYTE*)dest + result;
    }

    return result;
}

int LZ4_decompress_fast_continue (LZ4_streamDecode_t* LZ4_streamDecode, const char* source, char* dest, int originalSize)
{
    LZ4_streamDecode_t_internal* lz4sd = (LZ4_streamDecode_t_internal*) LZ4_streamDecode;
    int result;

    if (lz4sd->prefixEnd == (BYTE*)dest)
    {
        result = LZ4_decompress_generic(source, dest, 0, originalSize,
                                        endOnOutputSize, full, 0,
                                        usingExtDict, lz4sd->prefixEnd - lz4sd->prefixSize, lz4sd->externalDict, lz4sd->extDictSize);
        if (result <= 0) return result;
        lz4sd->prefixSize += originalSize;
        lz4sd->prefixEnd  += originalSize;
    }
    else
    {
        lz4sd->extDictSize = lz4sd->prefixSize;
        lz4sd->externalDict = (BYTE*)dest - lz4sd->extDictSize;
        result = LZ4_decompress_generic(source, dest, 0, originalSize,
                                        endOnOutputSize, full, 0,
                                        usingExtDict, (BYTE*)dest, lz4sd->externalDict, lz4sd->extDictSize);
        if (result <= 0) return result;
        lz4sd->prefixSize = originalSize;
        lz4sd->prefixEnd  = (BYTE*)dest + originalSize;
    }

    return result;
}


/*
Advanced decoding functions :
*_usingDict() :
    These decoding functions work the same as "_continue" ones,
    the dictionary must be explicitly provided within parameters
*/

FORCE_INLINE int LZ4_decompress_usingDict_generic(const char* source, char* dest, int compressedSize, int maxOutputSize, int safe, const char* dictStart, int dictSize)
{
    if (dictSize==0)
        return LZ4_decompress_generic(source, dest, compressedSize, maxOutputSize, safe, full, 0, noDict, (BYTE*)dest, NULL, 0);
    if (dictStart+dictSize == dest)
    {
        if (dictSize >= (int)(64 KB - 1))
            return LZ4_decompress_generic(source, dest, compressedSize, maxOutputSize, safe, full, 0, withPrefix64k, (BYTE*)dest-64 KB, NULL, 0);
        return LZ4_decompress_generic(source, dest, compressedSize, maxOutputSize, safe, full, 0, noDict, (BYTE*)dest-dictSize, NULL, 0);
    }
    return LZ4_decompress_generic(source, dest, compressedSize, maxOutputSize, safe, full, 0, usingExtDict, (BYTE*)dest, (const BYTE*)dictStart, dictSize);
}

int LZ4_decompress_safe_usingDict(const char* source, char* dest, int compressedSize, int maxOutputSize, const char* dictStart, int dictSize)
{
    return LZ4_decompress_usingDict_generic(source, dest, compressedSize, maxOutputSize, 1, dictStart, dictSize);
}

int LZ4_decompress_fast_usingDict(const char* source, char* dest, int originalSize, const char* dictStart, int dictSize)
{
    return LZ4_decompress_usingDict_generic(source, dest, 0, originalSize, 0, dictStart, dictSize);
}

/* debug function */
int LZ4_decompress_safe_forceExtDict(const char* source, char* dest, int compressedSize, int maxOutputSize, const char* dictStart, int dictSize)
{
    return LZ4_decompress_generic(source, dest, compressedSize, maxOutputSize, endOnInputSize, full, 0, usingExtDict, (BYTE*)dest, (const BYTE*)dictStart, dictSize);
}


/***************************************************
*  Obsolete Functions
***************************************************/
/* obsolete compression functions */
int LZ4_compress_limitedOutput(const char* source, char* dest, int inputSize, int maxOutputSize) { return LZ4_compress_default(source, dest, inputSize, maxOutputSize); }
int LZ4_compress(const char* source, char* dest, int inputSize) { return LZ4_compress_default(source, dest, inputSize, LZ4_compressBound(inputSize)); }
int LZ4_compress_limitedOutput_withState (void* state, const char* src, char* dst, int srcSize, int dstSize) { return LZ4_compress_fast_extState(state, src, dst, srcSize, dstSize, 1); }
int LZ4_compress_withState (void* state, const char* src, char* dst, int srcSize) { return LZ4_compress_fast_extState(state, src, dst, srcSize, LZ4_compressBound(srcSize), 1); }
int LZ4_compress_limitedOutput_continue (LZ4_stream_t* LZ4_stream, const char* src, char* dst, int srcSize, int maxDstSize) { return LZ4_compress_fast_continue(LZ4_stream, src, dst, srcSize, maxDstSize, 1); }
int LZ4_compress_continue (LZ4_stream_t* LZ4_stream, const char* source, char* dest, int inputSize) { return LZ4_compress_fast_continue(LZ4_stream, source, dest, inputSize, LZ4_compressBound(inputSize), 1); }

/*
These function names are deprecated and should no longer be used.
They are only provided here for compatibility with older user programs.
- LZ4_uncompress is totally equivalent to LZ4_decompress_fast
- LZ4_uncompress_unknownOutputSize is totally equivalent to LZ4_decompress_safe
*/
int LZ4_uncompress (const char* source, char* dest, int outputSize) { return LZ4_decompress_fast(source, dest, outputSize); }
int LZ4_uncompress_unknownOutputSize (const char* source, char* dest, int isize, int maxOutputSize) { return LZ4_decompress_safe(source, dest, isize, maxOutputSize); }


/* Obsolete Streaming functions */

int LZ4_sizeofStreamState() { return LZ4_STREAMSIZE; }

static void LZ4_init(LZ4_stream_t_internal* lz4ds, BYTE* base)
{
    MEM_INIT(lz4ds, 0, LZ4_STREAMSIZE);
    lz4ds->bufferStart = base;
}

int LZ4_resetStreamState(void* state, char* inputBuffer)
{
    if ((((size_t)state) & 3) != 0) return 1;   /* Error : pointer is not aligned on 4-bytes boundary */
    LZ4_init((LZ4_stream_t_internal*)state, (BYTE*)inputBuffer);
    return 0;
}

void* LZ4_create (char* inputBuffer)
{
    void* lz4ds = ALLOCATOR(8, LZ4_STREAMSIZE_U64);
    LZ4_init ((LZ4_stream_t_internal*)lz4ds, (BYTE*)inputBuffer);
    return lz4ds;
}

char* LZ4_slideInputBuffer (void* LZ4_Data)
{
    LZ4_stream_t_internal* ctx = (LZ4_stream_t_internal*)LZ4_Data;
    int dictSize = LZ4_saveDict((LZ4_stream_t*)LZ4_Data, (char*)ctx->bufferStart, 64 KB);
    return (char*)(ctx->bufferStart + dictSize);
}

/* Obsolete streaming decompression functions */

int LZ4_decompress_safe_withPrefix64k(const char* source, char* dest, int compressedSize, int maxOutputSize)
{
    return LZ4_decompress_generic(source, dest, compressedSize, maxOutputSize, endOnInputSize, full, 0, withPrefix64k, (BYTE*)dest - 64 KB, NULL, 64 KB);
}

int LZ4_decompress_fast_withPrefix64k(const char* source, char* dest, int originalSize)
{
    return LZ4_decompress_generic(source, dest, 0, originalSize, endOnOutputSize, full, 0, withPrefix64k, (BYTE*)dest - 64 KB, NULL, 64 KB);
}

#endif   /* LZ4_COMMONDEFS_ONLY */


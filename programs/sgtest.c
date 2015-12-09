/*
    sgTest - test tool for lz4sg
TODO:    Copyright (C) Yann Collet 2014-2015

    GPL v2 License

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

TODO:    You can contact the author at :
    - LZ4 source repository : https://github.com/Cyan4973/lz4
    - LZ4 public forum : https://groups.google.com/forum/#!forum/lz4c
*/

/**************************************
*  Compiler specific
**************************************/
#ifdef _MSC_VER    /* Visual Studio */
#  pragma warning(disable : 4127)        /* disable: C4127: conditional expression is constant */
#  pragma warning(disable : 4146)        /* disable: C4146: minus unsigned expression */
#endif

/* S_ISREG & gettimeofday() are not supported by MSVC */
#if defined(_MSC_VER) || defined(_WIN32)
#  define FUZ_LEGACY_TIMER 1
#endif

#pragma GCC diagnostic push  // require GCC 4.6
#pragma GCC diagnostic ignored "-Wcast-qual"

/**************************************
*  Includes
**************************************/
#include <stdlib.h>     /* malloc, free, drand48 */
#include <math.h>       /* log */
#include <stdio.h>      /* fprintf */
#include <string.h>     /* strcmp */
#include "lz4frame_static.h" /* error info and frame compatibility checks */
#include "xxhash.h"     /* XXH64 */
#include "lz4sg.h"

/* Use ftime() if gettimeofday() is not available on your target */
#if defined(FUZ_LEGACY_TIMER)
#  include <sys/timeb.h>   /* timeb, ftime */
#else
#  include <sys/time.h>    /* gettimeofday */
#endif


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
*  Constants
**************************************/
#ifndef LZ4_VERSION
#  define LZ4_VERSION ""
#endif

#define LZ4F_MAGIC_SKIPPABLE_START 0x184D2A50U

#define KB *(1U<<10)
#define MB *(1U<<20)
#define GB *(1U<<30)

static const U32 nbTestsDefault = 256 KB;
#define COMPRESSIBLE_NOISE_LENGTH (2 MB)
#define FUZ_COMPRESSIBILITY_DEFAULT 50
static const U32 prime1 = 2654435761U;
static const U32 prime2 = 2246822519U;



/**************************************
*  Macros
**************************************/
#define DISPLAY(...)          fprintf(stderr, __VA_ARGS__)
#define DISPLAYLEVEL(l, ...)  if (displayLevel>=l) { DISPLAY(__VA_ARGS__); }
#define DISPLAYUPDATE(l, ...) if (displayLevel>=l) { \
            if ((FUZ_GetMilliSpan(g_time) > refreshRate) || (displayLevel>=4)) \
            { g_time = FUZ_GetMilliStart(); DISPLAY(__VA_ARGS__); \
            if (displayLevel>=4) fflush(stdout); } }
static const U32 refreshRate = 150;
static U32 g_time = 0;


/*****************************************
*  Local Parameters
*****************************************/
static U32 no_prompt = 0;
static char* programName;
static U32 displayLevel = 2;
static U32 pause = 0;


/*********************************************************
*  Fuzzer functions
*********************************************************/
#if defined(FUZ_LEGACY_TIMER)

static U32 FUZ_GetMilliStart(void)
{
    struct timeb tb;
    U32 nCount;
    ftime( &tb );
    nCount = (U32) (((tb.time & 0xFFFFF) * 1000) +  tb.millitm);
    return nCount;
}

#else

static U32 FUZ_GetMilliStart(void)
{
    struct timeval tv;
    U32 nCount;
    gettimeofday(&tv, NULL);
    nCount = (U32) (tv.tv_usec/1000 + (tv.tv_sec & 0xfffff) * 1000);
    return nCount;
}

#endif


static U32 FUZ_GetMilliSpan(U32 nTimeStart)
{
    U32 nCurrent = FUZ_GetMilliStart();
    U32 nSpan = nCurrent - nTimeStart;
    if (nTimeStart > nCurrent)
        nSpan += 0x100000 * 1000;
    return nSpan;
}


#  define FUZ_rotl32(x,r) ((x << r) | (x >> (32 - r)))
unsigned int FUZ_rand(unsigned int* src)
{
    U32 rand32 = *src;
    rand32 *= prime1;
    rand32 += prime2;
    rand32  = FUZ_rotl32(rand32, 13);
    *src = rand32;
    return rand32 >> 5;
}

// draw random number from an 'exponential'-like distribution with the given mean, [min,max]
// inexact (min and max get higher weight than expected), used for independent buffer-size selection
// uses global rand48 seed
static unsigned int rnd_exponential(unsigned int mean, unsigned int min, unsigned int max)
{
    double value = -log(1 - drand48()) * mean;
    if (value > max)
        return max;
    if (value < min)
        return min;
    unsigned int int_value = (unsigned int)value;
    if (int_value < min)
        return min;
    return int_value;
} // rnd_exponential


#define FUZ_RAND15BITS  (FUZ_rand(seed) & 0x7FFF)
#define FUZ_RANDLENGTH  ( (FUZ_rand(seed) & 3) ? (FUZ_rand(seed) % 15) : (FUZ_rand(seed) % 510) + 15)
static void FUZ_fillCompressibleNoiseBuffer(void* buffer, unsigned bufferSize, double proba, U32* seed)
{
    BYTE* BBuffer = (BYTE*)buffer;
    unsigned pos = 0;
    U32 P32 = (U32)(32768 * proba);

    /* First Byte */
    BBuffer[pos++] = (BYTE)(FUZ_rand(seed));

    while (pos < bufferSize)
    {
        /* Select : Literal (noise) or copy (within 64K) */
        if (FUZ_RAND15BITS < P32)
        {
            /* Copy (within 64K) */
            unsigned match, end;
            unsigned length = FUZ_RANDLENGTH + 4;
            unsigned offset = FUZ_RAND15BITS + 1;
            if (offset > pos) offset = pos;
            if (pos + length > bufferSize) length = bufferSize - pos;
            match = pos - offset;
            end = pos + length;
            while (pos < end) BBuffer[pos++] = BBuffer[match++];
        }
        else
        {
            /* Literal (noise) */
            unsigned end;
            unsigned length = FUZ_RANDLENGTH;
            if (pos + length > bufferSize) length = bufferSize - pos;
            end = pos + length;
            while (pos < end) BBuffer[pos++] = (BYTE)(FUZ_rand(seed) >> 5);
        }
    }
}


static unsigned FUZ_highbit(U32 v32)
{
    unsigned nbBits = 0;
    if (v32==0) return 0;
    while (v32)
    {
        v32 >>= 1;
        nbBits ++;
    }
    return nbBits;
}

#define NELEMS(X)   ( sizeof(X) / sizeof(X[0]) )
#define DEFAULT_ACCEL 0
#define MAX_SG_BUFFERS  20
#define DECODE_GUARD_LENGTH (100)

#define UT_VERIFY(cond, false_cmd)    do { if (!(cond)) { DISPLAYLEVEL(1, "%s:%d VERIFY_ERROR: " #cond "\n", __FUNCTION__, __LINE__); false_cmd; } } while(0)

static void locateBuffDiff(const void* buff1, const void* buff2, size_t size)
{
    //NOTE: since we only scan when test failed, no point in working with SGLs - let's just copy prior to comparison
    int p=0;
    const BYTE* b1=(const BYTE*)buff1;
    const BYTE* b2=(const BYTE*)buff2;
    while (b1[p]==b2[p]) p++;
    DISPLAY("Error at pos %i/%i : %02X != %02X \n", p, (int)size, b1[p], b2[p]);
}

static int verify_basic_LZ4F_decompression(const void* compressedBuffer, int cSize, U64 crcOrig, void* decodedBuffer, const size_t decodedBufferSize)
{
    DISPLAYLEVEL(3, "%s (cSize %i, crcOrig %08x, decodedBufferSize %i)\n", __FUNCTION__, cSize, (unsigned int)crcOrig, (int)decodedBufferSize);
    LZ4F_decompressionContext_t dCtx = NULL;
    U64 crcDest;
    size_t compressedBufferSize = cSize;
    BYTE* op = (BYTE*) decodedBuffer;
    BYTE* const oend = (BYTE*) decodedBuffer + decodedBufferSize;
    const BYTE* ip = (const BYTE*) compressedBuffer;
    const BYTE* const iend = (const BYTE*) compressedBuffer + cSize;
    LZ4F_errorCode_t errorCode = LZ4F_createDecompressionContext(&dCtx, LZ4F_VERSION);
    UT_VERIFY(!LZ4F_isError(errorCode), return __LINE__);

    memset(decodedBuffer, 0, decodedBufferSize);

    DISPLAYLEVEL(3, "Single Block : \n");
    size_t destSize = decodedBufferSize;
    errorCode = LZ4F_decompress(dCtx, decodedBuffer, &destSize, compressedBuffer, &compressedBufferSize, NULL);
    UT_VERIFY(!LZ4F_isError(errorCode), return __LINE__);

    crcDest = XXH64(decodedBuffer, decodedBufferSize, 1);
    DISPLAYLEVEL(3, "Regenerated %i bytes (%08x)\n", (int )decodedBufferSize, (unsigned int)crcDest);
    UT_VERIFY(crcDest == crcOrig, return __LINE__);

    memset(decodedBuffer, 0, decodedBufferSize);

    DISPLAYLEVEL(4, "Reusing decompression context \n");
    {
        size_t iSize = compressedBufferSize - 4;
        const BYTE* cBuff = (const BYTE*) compressedBuffer;
        DISPLAYLEVEL(3, "Missing last 4 bytes : ");
        destSize = decodedBufferSize;
        errorCode = LZ4F_decompress(dCtx, decodedBuffer, &destSize, cBuff, &iSize, NULL);
        UT_VERIFY(!LZ4F_isError(errorCode), return __LINE__);
        UT_VERIFY(errorCode, return __LINE__);
        crcDest = XXH64(decodedBuffer, destSize, 1);
        DISPLAYLEVEL(3, "crcDest (%08x)\n", (unsigned int)crcDest);

        DISPLAYLEVEL(3, "indeed, request %u bytes \n", (unsigned )errorCode);
        cBuff += iSize;
        iSize = errorCode;
        errorCode = LZ4F_decompress(dCtx, decodedBuffer, &destSize, cBuff, &iSize, NULL);
        UT_VERIFY(errorCode == 0, return __LINE__);

        crcDest = XXH64(decodedBuffer, decodedBufferSize, 1);
        DISPLAYLEVEL(3, "crcDest (%08x)\n", (unsigned int)crcDest);
        UT_VERIFY(crcDest == crcOrig, return __LINE__);
    }
    {
        size_t oSize = 0;
        size_t iSize = 0;
        LZ4F_frameInfo_t fi;
        DISPLAYLEVEL(3, "Start by feeding 0 bytes, to get next input size : ");
        errorCode = LZ4F_decompress(dCtx, NULL, &oSize, ip, &iSize, NULL);
        UT_VERIFY(!LZ4F_isError(errorCode), return __LINE__);

        DISPLAYLEVEL(3, " %u  \n", (unsigned )errorCode);
        DISPLAYLEVEL(3, "get FrameInfo on null input : ");
        errorCode = LZ4F_getFrameInfo(dCtx, &fi, ip, &iSize);
        UT_VERIFY(errorCode == (size_t) -LZ4F_ERROR_frameHeader_incomplete, return __LINE__);

        DISPLAYLEVEL(3, " correctly failed : %s \n", LZ4F_getErrorName(errorCode));
        DISPLAYLEVEL(3, "get FrameInfo on not enough input : ");
        iSize = 6;
        errorCode = LZ4F_getFrameInfo(dCtx, &fi, ip, &iSize);
        UT_VERIFY(errorCode == (size_t) -LZ4F_ERROR_frameHeader_incomplete, return __LINE__);

        DISPLAYLEVEL(3, " correctly failed : %s \n", LZ4F_getErrorName(errorCode));
        ip += iSize;
        DISPLAYLEVEL(3, "get FrameInfo on enough input : ");
        iSize = 15 - iSize;
        errorCode = LZ4F_getFrameInfo(dCtx, &fi, ip, &iSize);
        UT_VERIFY(!LZ4F_isError(errorCode), return __LINE__);

        DISPLAYLEVEL(3, " correctly decoded \n");
        ip += iSize;
    }
    DISPLAYLEVEL(3, "Byte after byte : \n");
    while (ip < iend) {
        size_t oSize = oend - op;
        size_t iSize = 1;
        errorCode = LZ4F_decompress(dCtx, op, &oSize, ip, &iSize, NULL);
        UT_VERIFY(!LZ4F_isError(errorCode), return __LINE__);

        op += oSize;
        ip += iSize;
    }
    crcDest = XXH64(decodedBuffer, decodedBufferSize, 1);
    UT_VERIFY(crcDest == crcOrig, return __LINE__);

    DISPLAYLEVEL(3, "Regenerated %u/%u bytes \n", (unsigned )(op - (BYTE* )decodedBuffer), (unsigned )decodedBufferSize);
    errorCode = LZ4F_freeDecompressionContext(dCtx);
    UT_VERIFY(!LZ4F_isError(errorCode), return __LINE__);

    dCtx = NULL;
    return 0;
}


int basicTests(U32 seed, double compressibility)
{
    int testResult = 0;
    void* CNBuffer;
    void* compressedBuffer;
    void* decodedBuffer;
    U32 randState = seed;
    int cSize, testSize;
    LZ4F_preferences_t prefs;
    LZ4F_compressionContext_t cctx = NULL;
    U64 crcOrig;
    LZ4SG_in_t  sg_cin [MAX_SG_BUFFERS];
    LZ4SG_out_t sg_cout[MAX_SG_BUFFERS];
    LZ4SG_in_t  sg_din [MAX_SG_BUFFERS];
    LZ4SG_out_t sg_dout[MAX_SG_BUFFERS];
    size_t sg_in_len, sg_out_len;
    size_t maxDstSize = LZ4_SG_compressBound(COMPRESSIBLE_NOISE_LENGTH, NELEMS(sg_cin), NELEMS(sg_cout));
    size_t sourceSizeOut;

    /* Create compressible test buffer */
    memset(&prefs, 0, sizeof(prefs));
    CNBuffer = malloc(COMPRESSIBLE_NOISE_LENGTH);
    compressedBuffer = malloc(maxDstSize);
    decodedBuffer = malloc(COMPRESSIBLE_NOISE_LENGTH + DECODE_GUARD_LENGTH);
    FUZ_fillCompressibleNoiseBuffer(CNBuffer, COMPRESSIBLE_NOISE_LENGTH, compressibility, &randState);
    crcOrig = XXH64(CNBuffer, COMPRESSIBLE_NOISE_LENGTH, 1);

    /* Trivial tests : one input and one output buffers */
    testSize = COMPRESSIBLE_NOISE_LENGTH;
    DISPLAYLEVEL(3, "One input and one output buffers : \n");
    sg_cin[0].sg_base = CNBuffer;
    sg_cin[0].sg_len  = COMPRESSIBLE_NOISE_LENGTH;
    sg_in_len        = 1;
    sg_cout[0].sg_base = compressedBuffer;
    sg_cout[0].sg_len  = maxDstSize;
    sg_out_len        = 1;
    sourceSizeOut = testSize;
    cSize = LZ4_SG_compress(&sg_cin[0], sg_in_len, &sg_cout[0], sg_out_len, &sourceSizeOut, maxDstSize, DEFAULT_ACCEL);
    DISPLAYLEVEL(3, "Compressed %i bytes into a %i bytes frame \n", (int)testSize, (int)cSize);
    UT_VERIFY(cSize > 0, goto _output_error);

    DISPLAYLEVEL(3, "Decompress test various valid sg_out and maxDstSize combinations \n");
    {
        U64 crcDest;
        // sg_out.sg_len == maxDstSize == originalSize
        sg_din[0].sg_base = compressedBuffer;
        sg_din[0].sg_len  = cSize;
        sg_dout[0].sg_base = decodedBuffer;
        sg_dout[0].sg_len  = COMPRESSIBLE_NOISE_LENGTH;
        sourceSizeOut = cSize;
        maxDstSize = testSize;
        memset(decodedBuffer, 0, testSize);
        testResult = LZ4_SG_decompress(&sg_din[0], sg_in_len, &sg_dout[0], sg_out_len, &sourceSizeOut, maxDstSize);
        crcDest = XXH64(decodedBuffer, COMPRESSIBLE_NOISE_LENGTH, 1);
        DISPLAYLEVEL(3, "Decompressed %i bytes (out of %i bytes) to a %i bytes (out of %i bytes) dst_limit %i bytes\n", (int)sourceSizeOut, (int)cSize, (int)testResult, (int)testSize, (int)maxDstSize);
        UT_VERIFY(testResult >  0       , goto _output_error);
        UT_VERIFY(testResult == testSize, goto _output_error);
        UT_VERIFY(   crcDest == crcOrig , goto _output_error);

        // maxDstSize > originalSize
        maxDstSize = COMPRESSIBLE_NOISE_LENGTH + DECODE_GUARD_LENGTH;
        sourceSizeOut = cSize;
        memset(decodedBuffer, 0, testSize);
        testResult = LZ4_SG_decompress(&sg_din[0], sg_in_len, &sg_dout[0], sg_out_len, &sourceSizeOut, maxDstSize);
        crcDest = XXH64(decodedBuffer, COMPRESSIBLE_NOISE_LENGTH, 1);
        DISPLAYLEVEL(3, "Decompressed %i bytes (out of %i bytes) to a %i bytes (out of %i bytes) dst_limit %i bytes\n", (int)sourceSizeOut, (int)cSize, (int)testResult, (int)testSize, (int)maxDstSize);
        UT_VERIFY(testResult >  0       , goto _output_error);
        UT_VERIFY(testResult == testSize, goto _output_error);
        UT_VERIFY(   crcDest == crcOrig , goto _output_error);

        // sg_out.sg_len > originalSize
        maxDstSize = testSize;
        sg_dout[0].sg_len  = COMPRESSIBLE_NOISE_LENGTH + DECODE_GUARD_LENGTH;
        sourceSizeOut = cSize;
        memset(decodedBuffer, 0, testSize);
        testResult = LZ4_SG_decompress(&sg_din[0], sg_in_len, &sg_dout[0], sg_out_len, &sourceSizeOut, maxDstSize);
        crcDest = XXH64(decodedBuffer, COMPRESSIBLE_NOISE_LENGTH, 1);
        DISPLAYLEVEL(3, "Decompressed %i bytes (out of %i bytes) to a %i bytes (out of %i bytes) dst_limit %i bytes\n", (int)sourceSizeOut, (int)cSize, (int)testResult, (int)testSize, (int)maxDstSize);
        UT_VERIFY(testResult >  0       , goto _output_error);
        UT_VERIFY(testResult == testSize, goto _output_error);
        UT_VERIFY(   crcDest == crcOrig , goto _output_error);
    }

    DISPLAYLEVEL(3, "Frame Decompression test (%08x): \n", (unsigned int)crcOrig);
    {
        int lz4f_result = verify_basic_LZ4F_decompression(compressedBuffer, cSize, crcOrig, decodedBuffer, testSize);
        UT_VERIFY(0 == lz4f_result, goto _output_error);
    }

    // basic SGL test
    {
        // prepare SGL input
        const size_t num_data_buffers = 16;
        const size_t buf_size_bytes = 4 KB;
        unsigned int i;
        for (i = 0; i < 1+num_data_buffers; i++) {
            sg_cin [i].sg_base = malloc(buf_size_bytes);
            sg_cin [i].sg_len  = buf_size_bytes;
            sg_cout[i].sg_base = malloc(buf_size_bytes);
            sg_cout[i].sg_len  = buf_size_bytes;
            sg_din [i].sg_base = malloc(buf_size_bytes);
            sg_din [i].sg_len  = buf_size_bytes;
            sg_dout[i].sg_base = malloc(buf_size_bytes);
            sg_dout[i].sg_len  = buf_size_bytes;
        }
        for (i = 0; i < num_data_buffers; i++) {
            memcpy((void *)sg_cin [i].sg_base, ((const char *)CNBuffer)+(i*buf_size_bytes), buf_size_bytes);
        }
        sg_in_len  = num_data_buffers;
        sg_out_len = 1+num_data_buffers;
        testSize = num_data_buffers * buf_size_bytes;
        maxDstSize = (1+num_data_buffers) * buf_size_bytes;
        crcOrig = XXH64(CNBuffer, testSize, 1);

        DISPLAYLEVEL(3, "Compress 16 4KB buffers into 17 4KB buffers : \n");
        sourceSizeOut = testSize;
        cSize = LZ4_SG_compress(&sg_cin[0], sg_in_len, &sg_cout[0], sg_out_len, &sourceSizeOut, maxDstSize, DEFAULT_ACCEL);
        DISPLAYLEVEL(3, "Compressed %i bytes into a %i bytes frame \n", (int)testSize, (int)cSize);
        UT_VERIFY(cSize > 0, goto _output_error);

        DISPLAYLEVEL(3, "Decompress 17 4KB buffers into 16 4KB buffers : \n");
        for (i = 0; i < 1+num_data_buffers; i++) {
            memcpy((void *)sg_din[i].sg_base, sg_cout[i].sg_base, buf_size_bytes);
        }
        sourceSizeOut = cSize;
        maxDstSize = testSize;
        sg_in_len  = 1+num_data_buffers;
        sg_out_len = num_data_buffers;
        memset(decodedBuffer, 0, testSize);
        testResult = LZ4_SG_decompress(&sg_din[0], sg_in_len, &sg_dout[0], sg_out_len, &sourceSizeOut, maxDstSize);
        DISPLAYLEVEL(3, "Decompressed %i bytes (out of %i bytes) to a %i bytes (out of %i bytes) dst_limit %i bytes\n", (int)sourceSizeOut, (int)cSize, (int)testResult, (int)testSize, (int)maxDstSize);
        for (i = 0; i < num_data_buffers; i++) {
            memcpy(((char *)decodedBuffer)+(i*buf_size_bytes), sg_dout[i].sg_base, buf_size_bytes);
        }
        UT_VERIFY(testResult >  0       , goto _output_error);
        UT_VERIFY(testResult == testSize, goto _output_error);

        U64 crcDest;
        crcDest = XXH64(decodedBuffer, testSize, 1);
        UT_VERIFY(   crcDest == crcOrig , goto _output_error);

        DISPLAYLEVEL(3, "verify frame decompress on concatenated buffer: \n");
        for (i = 0; i < 1+num_data_buffers; i++) {
            memcpy(((char *)compressedBuffer)+(i*buf_size_bytes), sg_cout[i].sg_base, buf_size_bytes);
        }
        int lz4f_result = verify_basic_LZ4F_decompression(compressedBuffer, cSize, crcOrig, decodedBuffer, testSize);
        UT_VERIFY(0 == lz4f_result, goto _output_error);

        // release SGL
        for (i = 0; i < 1+num_data_buffers; i++) {
            free((void *)sg_cin [i].sg_base); sg_cin [i].sg_base = NULL; sg_cin [i].sg_len = 0;
            free(sg_cout[i].sg_base); sg_cout[i].sg_base = NULL; sg_cout[i].sg_len = 0;
            free((void *)sg_din [i].sg_base); sg_din [i].sg_base = NULL; sg_din [i].sg_len = 0;
            free(sg_dout[i].sg_base); sg_dout[i].sg_base = NULL; sg_dout[i].sg_len = 0;
        }
    }

    DISPLAY("Basic tests completed \n");
    testResult = 0;
_end:
    free(CNBuffer);
    free(compressedBuffer);
    free(decodedBuffer);
    LZ4F_freeCompressionContext(cctx); cctx = NULL;
    return testResult;

_output_error:
    testResult = 1;
    DISPLAY("Error detected ! \n");
    locateBuffDiff(CNBuffer, decodedBuffer, testSize);
    goto _end;

    // unreachable
    return -1;
}


static const U32 srcDataLength = 9 MB;  /* needs to be > 2x4MB to test large blocks */

int fuzzerTests(U32 seed, unsigned nbTests, unsigned startTest, double compressibility, U32 duration)
{
    unsigned testResult = 0;
    unsigned testNb = 0;
    void* srcBuffer = NULL;
    void* compressedBuffer = NULL;
    void* decodedBuffer = NULL;
    U32 coreRand = seed;
    LZ4F_decompressionContext_t dCtx = NULL;
    LZ4F_compressionContext_t cCtx = NULL;
    size_t result;
    const U32 startTime = FUZ_GetMilliStart();
    XXH64_state_t xxh64;
#   define CHECK(cond, ...) if (cond) { DISPLAY("Error => "); DISPLAY(__VA_ARGS__); \
                            DISPLAY(" (seed %u, test nb %u)  \n", seed, testNb); goto _output_error; }

    // backup all allocated addresses, from which we will later select buffers
    const size_t max_buf_size = 131 KB;
    size_t num_buf_size_distribution_deviations = 0;

    LZ4SG_in_t  sg_in_buf_potential [2*MAX_SG_BUFFERS];
    LZ4SG_out_t sg_out_buf_potential[2*MAX_SG_BUFFERS];

    LZ4SG_in_t  sg_cin [MAX_SG_BUFFERS];
    LZ4SG_out_t sg_cout[MAX_SG_BUFFERS];
    LZ4SG_in_t  sg_din [MAX_SG_BUFFERS];
    LZ4SG_out_t sg_dout[MAX_SG_BUFFERS];
    size_t sg_cin_len, sg_cout_len, sg_din_len, sg_dout_len;
    const size_t maxDstSize = LZ4_SG_compressBound(srcDataLength, NELEMS(sg_cin), NELEMS(sg_cout));

    unsigned int i;
    for (i = 0; i < NELEMS(sg_in_buf_potential); i++) {
        sg_in_buf_potential [i].sg_base = malloc(max_buf_size);
        sg_in_buf_potential [i].sg_len  = max_buf_size;
        sg_out_buf_potential[i].sg_base = malloc(max_buf_size);
        sg_out_buf_potential[i].sg_len  = max_buf_size;
    }

    /* Init */
    duration *= 1000;

    /* Create buffers */
    result = LZ4F_createDecompressionContext(&dCtx, LZ4F_VERSION);
    CHECK(LZ4F_isError(result), "Allocation failed (error %i)", (int)result);
    result = LZ4F_createCompressionContext(&cCtx, LZ4F_VERSION);
    CHECK(LZ4F_isError(result), "Allocation failed (error %i)", (int)result);
    srcBuffer = malloc(srcDataLength);
    CHECK(srcBuffer==NULL, "srcBuffer Allocation failed");
    const size_t compressedBufferLength = maxDstSize;
    compressedBuffer = malloc(compressedBufferLength);
    CHECK(compressedBuffer==NULL, "compressedBuffer Allocation failed");
    decodedBuffer = calloc(1, srcDataLength);   /* calloc avoids decodedBuffer being considered "garbage" by scan-build */
    CHECK(decodedBuffer==NULL, "decodedBuffer Allocation failed");
    FUZ_fillCompressibleNoiseBuffer(srcBuffer, srcDataLength, compressibility, &coreRand);

    /* jump to requested testNb */
    for (testNb =0; (testNb < startTest); testNb++) (void)FUZ_rand(&coreRand);   // sync randomizer

    /* main fuzzer test loop */
    for ( ; (testNb < nbTests) || (duration > FUZ_GetMilliSpan(startTime)) ; testNb++)
    {
        U32 randState = coreRand ^ prime1;
        (void)FUZ_rand(&coreRand);   /* update seed */

        srand48(FUZ_rand(&randState));

        DISPLAYUPDATE(2, "\r%5u   ", testNb);

        const size_t max_src_buf_size = (4 MB > srcDataLength) ? srcDataLength : 4 MB;
        unsigned nbBits = (FUZ_rand(&randState) % (FUZ_highbit(max_src_buf_size-1) - 1)) + 1;
        const size_t min_src_size = 20;
        const size_t min_first_dest_buf_size = 21;
        const size_t min_src_buf_size = 1;
        const size_t min_dst_buf_size = 10;
        size_t srcSize = (FUZ_rand(&randState) & ((1<<nbBits)-1)) + min_src_size;
        size_t srcStart = FUZ_rand(&randState) % (srcDataLength - srcSize);
        size_t cSize;
        size_t dstSize;
        size_t dstSizeBound;
        U64 crcOrig, crcDecoded;

        unsigned int test_selection = FUZ_rand(&randState);
        //TODO: enable lz4f_compress_compatibility_test with LZ4_SG_decompress
        int lz4f_compress_compatibility_test = 0;//(test_selection % 4) == 0;

        if (!lz4f_compress_compatibility_test)
        {
            // SGL compress
            unsigned int buffer_selection = FUZ_rand(&randState);

            if ((buffer_selection & 0xF) == 1)
            {
                // SG compress single source and single target buffers
                sg_cin[0].sg_base = (BYTE*)srcBuffer+srcStart;
                sg_cin[0].sg_len  = srcSize;
                sg_cin_len = 1;
                sg_cout[0].sg_base = compressedBuffer;
                sg_cout[0].sg_len  = compressedBufferLength;
                sg_cout_len = 1;
                dstSizeBound = dstSize = compressedBufferLength;
            }
            else
            {
                // SG compress random number and size source and target buffers
                sg_cin_len  = 1 + (FUZ_rand(&randState) % MAX_SG_BUFFERS);
                sg_cout_len = 1 + (FUZ_rand(&randState) % MAX_SG_BUFFERS);

                // single source buffer
                if (1 == sg_cin_len) {
                    sg_cin[0].sg_base = (BYTE*)srcBuffer+srcStart;
                    sg_cin[0].sg_len  = srcSize;

                    DISPLAYUPDATE(4, "INFO: single source buf size %i\n", (int)srcSize);
                }
                else {
                    // multiple source buffers
                    if (srcSize > sg_cin_len*max_buf_size/2) {
                        srcSize = sg_cin_len*max_buf_size/2;
                        num_buf_size_distribution_deviations++;
                        DISPLAYUPDATE(4, "NOTE: source buffer total size deviation %i\n", (int)num_buf_size_distribution_deviations);
                    }

                    size_t exact_src_size = 0;
                    unsigned int buf_size_mean = srcSize / sg_cin_len;
                    for (i = 0; i < sg_cin_len; i++) {
                        size_t buf_size = rnd_exponential(buf_size_mean, min_src_buf_size, max_buf_size);
                        DISPLAYUPDATE(4, "INFO: source buf %i size %i\n", i, (int)buf_size);

                        if (srcStart+exact_src_size+buf_size > srcDataLength) {
                            buf_size = srcDataLength-(srcStart+exact_src_size);
                        }
                        sg_cin[i].sg_base = sg_in_buf_potential[i*2+1].sg_base;
                        sg_cin[i].sg_len  = buf_size;
                        memcpy((void *)sg_cin[i].sg_base, (BYTE*)srcBuffer+srcStart+exact_src_size, buf_size);
                        exact_src_size += buf_size;
                        if (srcStart+exact_src_size == srcDataLength) {
                            num_buf_size_distribution_deviations++;
                            sg_cin_len = i+1;
                            DISPLAYUPDATE(4, "NOTE: final source buffer size deviation %i (buffers number limited to %i)\n", (int)num_buf_size_distribution_deviations, (int)sg_cin_len);
                        }
                    }
                    srcSize = exact_src_size;
                }

                // we can now derive the required limit for output
                dstSizeBound = LZ4_SG_compressBound(srcSize, sg_cin_len, sg_cout_len);

                // single target buffer
                if (1 == sg_cout_len) {
                    sg_cout[0].sg_base = compressedBuffer;
                    sg_cout[0].sg_len  = compressedBufferLength;
                }
                else {
                    // multiple target buffers
                    int finalBufferTruncated = 0;
                    dstSize = 0;
                    unsigned int buf_size_mean = dstSizeBound / sg_cout_len;
                    for (i = 0; i < sg_cout_len; i++) {
                        const size_t min_buf_size = (i == 0) ? min_first_dest_buf_size : min_dst_buf_size;
                        size_t buf_size = rnd_exponential(buf_size_mean, min_buf_size, max_buf_size);
                        DISPLAYUPDATE(4, "INFO: target buf %i size %i\n", (int)i, (int)buf_size);

                        if (dstSize+buf_size > dstSizeBound) {
                            buf_size = dstSizeBound-dstSize;
                            finalBufferTruncated = 1;
                        }
                        dstSize += buf_size;

                        sg_cout[i].sg_base = sg_out_buf_potential[i*2+1].sg_base;
                        sg_cout[i].sg_len  = buf_size;
                        if (finalBufferTruncated) {
                            num_buf_size_distribution_deviations++;

                            if (buf_size < min_buf_size) {
                                // merge truncated with previous?
                                if (i > 0) {
                                    sg_cout[i-1].sg_len += buf_size;
                                    if (sg_cout[i-1].sg_len > max_buf_size) {
                                        // skip, too much hassle
                                        DISPLAYUPDATE(4, "NOTE: unable to truncate final target buffer size (deviations %i), skipping\n", (int)num_buf_size_distribution_deviations);
                                        sg_cout_len = 0; break;
                                    }
                                }
                                else {
                                    // can this happen?
                                    DISPLAYUPDATE(4, "NOTE: unable to truncate first and final target buffer size (deviations %i), skipping\n", (int)num_buf_size_distribution_deviations);
                                    sg_cout_len = 0; break;
                                }
                                sg_cout_len = i;
                            }
                            else {
                                sg_cout_len = i+1;
                            }
                            DISPLAYUPDATE(4, "NOTE: final target buffer size truncated (%i), buffers number limited to %i, final's size is now %i (deviations %i)\n",
                                    (int)buf_size, (int)sg_cout_len, (int)sg_cout[sg_cout_len-1].sg_len, (int)num_buf_size_distribution_deviations);
                        }
                    }

                    // skip/abort condition
                    if (0 == sg_cout_len) continue;
                }

                if ((buffer_selection & 0xF) == 0) {
                    //TODO: select a random input and output buffer and split it in two,
                    // feeding consecutive addresses as consecutive entries in SGL

                }
            }

            crcOrig = XXH64((BYTE*)srcBuffer+srcStart, srcSize, 1);

            size_t sourceSizeOut = srcSize;
            result = LZ4_SG_compress(&sg_cin[0], sg_cin_len, &sg_cout[0], sg_cout_len, &sourceSizeOut, maxDstSize, DEFAULT_ACCEL);
            if (((result == 0) || (sourceSizeOut != srcSize)) && (dstSize < dstSizeBound)) {
                // forgive compression failure when output total size is lower than bound
                num_buf_size_distribution_deviations++;
                DISPLAYUPDATE(4, "NOTE: dstSize %i < %i dstSizeBound, compression attempt failed, not totally unexpected (deviations %i), skipping\n",
                        (int)dstSize, (int)dstSizeBound, (int)num_buf_size_distribution_deviations);
                continue;
            }

            CHECK(result <= 0, "Compression failed (error %i)", (int)result);
            CHECK(sourceSizeOut != srcSize, "Compression stopped at %i out of %i", (int)sourceSizeOut, (int)srcSize);
            cSize = result;
        }
        else
        {
            // LZ4F compression - use it in order to verify SGL decompress compatibility with it
            DISPLAYUPDATE(4, "INFO: LZ4F compression\n");

// alternative
//            size_t dstMaxSize = LZ4F_compressFrameBound(srcSize, prefsPtr);
//            DISPLAYLEVEL(3, "compressFrame srcSize %zu dstMaxSize %zu\n",
//                    srcSize, dstMaxSize);
//            cSize = LZ4F_compressFrame(compressedBuffer, dstMaxSize, (char*)srcBuffer + srcStart, srcSize, prefsPtr);
//            CHECK(LZ4F_isError(cSize), "LZ4F_compressFrame failed : error %i (%s)", (int)cSize, LZ4F_getErrorName(cSize));

            crcOrig = XXH64((BYTE*)srcBuffer+srcStart, srcSize, 1);

            unsigned BSId   = 4 + (FUZ_rand(&randState) & 3);
            unsigned BMId   = FUZ_rand(&randState) & 1;
            unsigned CCflag = FUZ_rand(&randState) & 1;
            unsigned autoflush = (FUZ_rand(&randState) & 7) == 2;
            U64 frameContentSize = ((FUZ_rand(&randState) & 0xF) == 1) ? srcSize : 0;
            LZ4F_preferences_t prefs;
            LZ4F_compressOptions_t cOptions;

            LZ4F_preferences_t* prefsPtr = &prefs;
            memset(&prefs, 0, sizeof(prefs));
            memset(&cOptions, 0, sizeof(cOptions));
            prefs.frameInfo.blockMode = (LZ4F_blockMode_t)BMId;
            prefs.frameInfo.blockSizeID = (LZ4F_blockSizeID_t)BSId;
            prefs.frameInfo.contentChecksumFlag = (LZ4F_contentChecksum_t)CCflag;
            prefs.frameInfo.contentSize = frameContentSize;
            prefs.autoFlush = autoflush;
            prefs.compressionLevel = FUZ_rand(&randState) % 5;
            if ((FUZ_rand(&randState) & 0xF) == 1) prefsPtr = NULL;

            const BYTE* ip = (const BYTE*)srcBuffer + srcStart;
            const BYTE* const iend = ip + srcSize;
            BYTE* op = (BYTE*)compressedBuffer;
            BYTE* const oend = op + LZ4F_compressFrameBound(srcDataLength, NULL);
            unsigned maxBits = FUZ_highbit((U32)srcSize);
            result = LZ4F_compressBegin(cCtx, op, oend-op, prefsPtr);
            CHECK(LZ4F_isError(result), "Compression header failed (error %i)", (int)result);
            op += result;
            while (ip < iend)
            {
                unsigned nbBitsSeg = FUZ_rand(&randState) % maxBits;
                size_t iSize = (FUZ_rand(&randState) & ((1<<nbBitsSeg)-1)) + 1;
                size_t oSize = LZ4F_compressBound(iSize, prefsPtr);
                unsigned forceFlush = ((FUZ_rand(&randState) & 3) == 1);
                if (iSize > (size_t)(iend-ip)) iSize = iend-ip;
                cOptions.stableSrc = ((FUZ_rand(&randState) & 3) == 1);

                DISPLAYLEVEL(3, "compressUpdate ip %d iSize %zu oSize %zu forceFlush %d\n",
                        (int)(ip-((const BYTE*)srcBuffer + srcStart)), iSize, oSize, forceFlush);
                result = LZ4F_compressUpdate(cCtx, op, oSize, ip, iSize, &cOptions);
                CHECK(LZ4F_isError(result), "Compression failed (error %i)", (int)result);
                op += result;
                ip += iSize;

                if (forceFlush)
                {
                    result = LZ4F_flush(cCtx, op, oend-op, &cOptions);
                    CHECK(LZ4F_isError(result), "Compression failed (error %i)", (int)result);
                    op += result;
                }
            }
            result = LZ4F_compressEnd(cCtx, op, oend-op, &cOptions);
            CHECK(LZ4F_isError(result), "Compression completion failed (error %i)", (int)result);
            op += result;
            cSize = op-(BYTE*)compressedBuffer;
        }

        //DECOMPRESS
        test_selection = FUZ_rand(&randState);

        if (lz4f_compress_compatibility_test || ((test_selection % 2) == 0))
        {
            //TODO: SGL decompress with random buffer sizes

            // SGL decompress with same buffer sizes used for compression
            // prepare din with cout's data
            sg_din_len  = sg_cout_len;
            for (i = 0; i < sg_din_len; i++) {
                sg_din[i].sg_len  = sg_cout[i].sg_len;
                if (sg_cout[i].sg_len <= max_buf_size) {
                    // enough room to copy - do it
                    sg_din[i].sg_base = sg_in_buf_potential[i*2+0].sg_base;
                    if (sg_din[i].sg_base != sg_cout[i].sg_base) {
                        memcpy((void *)sg_din[i].sg_base, sg_cout[i].sg_base, sg_cout[i].sg_len);
                    }
                }
                else {
                    // this is probably single output buffer - skip copy, use directly
                    sg_din[i].sg_base = sg_cout[i].sg_base;
                }
            }
            // prepare dout to receive decompressed data
            sg_dout_len = sg_cin_len;
            for (i = 0; i < sg_dout_len; i++) {
                sg_dout[i].sg_len  = sg_cin[i].sg_len;
                if (sg_cin[i].sg_len <= max_buf_size) {
                    // enough room to decompress into independent buffer
                    sg_dout[i].sg_base = sg_out_buf_potential[i*2+0].sg_base;
                }
                else {
                    // this is probably single input buffer, use an external output buffer
                    sg_dout[i].sg_base = decodedBuffer;
                }
            }

            size_t sourceSizeOut = cSize;
            size_t maxOutputSize = srcSize;
            int decomp_result = LZ4_SG_decompress(&sg_din[0], sg_din_len, &sg_dout[0], sg_dout_len, &sourceSizeOut, maxOutputSize);
            CHECK(decomp_result <= 0, "SG decompression failed (error %i)", (int)decomp_result);
            CHECK(decomp_result != (int)srcSize, "SG decompression stopped at  %i", (int)decomp_result);

            // verify result checksum
            size_t total_checked = 0;
            XXH64_reset(&xxh64, 1);
            for (i = 0; (i < sg_dout_len) && ((int)total_checked < decomp_result); i++) {
                size_t cur_size = sg_dout[i].sg_len;
                size_t rem = decomp_result - total_checked;
                if (rem < cur_size) cur_size = rem;
                total_checked += cur_size;

                XXH64_update(&xxh64, sg_dout[i].sg_base, cur_size);
            }
            crcDecoded = XXH64_digest(&xxh64);
            if (crcDecoded != crcOrig) {
                DISPLAYLEVEL(1, "checked %i out of %i (crcDecoded %08x, crcOrig %08x)\n",
                        (int)total_checked, decomp_result, (unsigned)crcDecoded, (unsigned)crcOrig);
                // locate error if any
                total_checked = 0;
                for (i = 0; (i < sg_dout_len) && ((int)total_checked < decomp_result); i++) {
                    size_t cur_size = sg_dout[i].sg_len;
                    size_t rem = decomp_result - total_checked;
                    if (rem < cur_size) cur_size = rem;
                    total_checked += cur_size;

                    U64 crc_in  = XXH64(sg_cin [i].sg_base, cur_size, 1);
                    U64 crc_out = XXH64(sg_dout[i].sg_base, cur_size, 1);
                    if (crc_in != crc_out) {
                        locateBuffDiff(sg_cin[i].sg_base, sg_dout[i].sg_base, cur_size);
                        break;
                    }
                }
                DISPLAYLEVEL(1, "checked %i out of %i\n",
                        (int)total_checked, decomp_result);
            }
            CHECK(crcDecoded != crcOrig, "Decompression corruption");
        }
        else
        {
            // prepare compressedBuffer from SGL
            size_t total_copied = 0;
            for (i = 0; i < sg_cout_len; i++) {
                size_t buf_size_bytes = cSize - total_copied;
                if (buf_size_bytes == 0) break;
                if (buf_size_bytes > sg_cout[i].sg_len) buf_size_bytes = sg_cout[i].sg_len;
                if (((char *)compressedBuffer)+total_copied != sg_cout[i].sg_base) {
                    memcpy(((char *)compressedBuffer)+total_copied, sg_cout[i].sg_base, buf_size_bytes);
                }
                total_copied += buf_size_bytes;
            }

            LZ4F_decompressOptions_t dOptions;
            memset(&dOptions, 0, sizeof(dOptions));

            const BYTE* ip = (const BYTE*)compressedBuffer;
            const BYTE* const iend = ip + cSize;
            BYTE* op = (BYTE*)decodedBuffer;
            BYTE* const oend = op + srcDataLength;
            size_t totalOut = 0;
            unsigned maxBits = FUZ_highbit((U32)cSize);
            XXH64_reset(&xxh64, 1);
            if (maxBits < 3) maxBits = 3;
            while (ip < iend)
            {
                unsigned nbBitsI = (FUZ_rand(&randState) % (maxBits-1)) + 1;
                unsigned nbBitsO = (FUZ_rand(&randState) % (maxBits)) + 1;
                size_t iSize = (FUZ_rand(&randState) & ((1<<nbBitsI)-1)) + 1;
                size_t oSize = (FUZ_rand(&randState) & ((1<<nbBitsO)-1)) + 2;
                if (iSize > (size_t)(iend-ip)) iSize = iend-ip;
                if (oSize > (size_t)(oend-op)) oSize = oend-op;
                dOptions.stableDst = FUZ_rand(&randState) & 1;
                result = LZ4F_decompress(dCtx, op, &oSize, ip, &iSize, &dOptions);
                if (result == (size_t)-LZ4F_ERROR_contentChecksum_invalid)
                    locateBuffDiff((BYTE*)srcBuffer+srcStart, decodedBuffer, srcSize);
                CHECK(LZ4F_isError(result), "Decompression failed (error %i:%s ip %d)",
                        (int)result, LZ4F_getErrorName((LZ4F_errorCode_t)result), (int)(ip-(const BYTE*)compressedBuffer));
                XXH64_update(&xxh64, op, (U32)oSize);
                totalOut += oSize;
                op += oSize;
                ip += iSize;
            }
            CHECK(result != 0, "Frame decompression failed (error %i)", (int)result);
            if (totalOut)   /* otherwise, it's a skippable frame */
            {
                crcDecoded = XXH64_digest(&xxh64);
                if (crcDecoded != crcOrig) locateBuffDiff((BYTE*)srcBuffer+srcStart, decodedBuffer, srcSize);
                CHECK(crcDecoded != crcOrig, "Decompression corruption");
            }
        }
    }

    DISPLAYLEVEL(2, "\rAll tests completed   \n");

_end:
    LZ4F_freeDecompressionContext(dCtx);
    LZ4F_freeCompressionContext(cCtx);
    free(srcBuffer);
    free(compressedBuffer);
    free(decodedBuffer);
    for (i = 0; i < NELEMS(sg_in_buf_potential); i++) {
        free((void *)(sg_in_buf_potential [i].sg_base));
        free(         sg_out_buf_potential[i].sg_base);
    }

    if (num_buf_size_distribution_deviations > 0) {
        DISPLAYLEVEL(2, "NOTE: %i buffer size deviations \n", (int)num_buf_size_distribution_deviations);
    }

    if (pause)
    {
        DISPLAY("press enter to finish \n");
        (void)getchar();
    }
    return testResult;

_output_error:
    testResult = 1;
    goto _end;

    // unreachable
    return -1;
#undef CHECK
}


int FUZ_usage(void)
{
    DISPLAY( "Usage :\n");
    DISPLAY( "      %s [args]\n", programName);
    DISPLAY( "\n");
    DISPLAY( "Arguments :\n");
    DISPLAY( " -i#    : Nb of tests (default:%u) \n", nbTestsDefault);
    DISPLAY( " -T#    : Duration of tests, in seconds (default: use Nb of tests) \n");
    DISPLAY( " -s#    : Select seed (default:prompt user)\n");
    DISPLAY( " -t#    : Select starting test number (default:0)\n");
    DISPLAY( " -P#    : Select compressibility in %% (default:%i%%)\n", FUZ_COMPRESSIBILITY_DEFAULT);
    DISPLAY( " -v     : verbose\n");
    DISPLAY( " -h     : display help and exit\n");
    return 0;
}


int main(int argc, char** argv)
{
    U32 seed=0;
    int seedset=0;
    int argNb;
    int nbTests = nbTestsDefault;
    int testNb = 0;
    int proba = FUZ_COMPRESSIBILITY_DEFAULT;
    int result=0;
    U32 duration=0;

    /* Check command line */
    programName = argv[0];
    for(argNb=1; argNb<argc; argNb++)
    {
        char* argument = argv[argNb];

        if(!argument) continue;   /* Protection if argument empty */

        /* Decode command (note : aggregated commands are allowed) */
        if (argument[0]=='-')
        {
            if (!strcmp(argument, "--no-prompt"))
            {
                no_prompt=1;
                seedset=1;
                displayLevel=1;
                continue;
            }
            argument++;

            while (*argument!=0)
            {
                switch(*argument)
                {
                case 'h':
                    return FUZ_usage();
                case 'v':
                    argument++;
                    displayLevel=4;
                    break;
                case 'q':
                    argument++;
                    displayLevel--;
                    break;
                case 'p': /* pause at the end */
                    argument++;
                    pause = 1;
                    break;

                case 'i':
                    argument++;
                    nbTests=0; duration=0;
                    while ((*argument>='0') && (*argument<='9'))
                    {
                        nbTests *= 10;
                        nbTests += *argument - '0';
                        argument++;
                    }
                    break;

                case 'T':
                    argument++;
                    nbTests = 0; duration = 0;
                    for (;;)
                    {
                        switch(*argument)
                        {
                            case 'm': duration *= 60; argument++; continue;
                            case 's':
                            case 'n': argument++; continue;
                            case '0':
                            case '1':
                            case '2':
                            case '3':
                            case '4':
                            case '5':
                            case '6':
                            case '7':
                            case '8':
                            case '9': duration *= 10; duration += *argument++ - '0'; continue;
                        }
                        break;
                    }
                    break;

                case 's':
                    argument++;
                    seed=0;
                    seedset=1;
                    while ((*argument>='0') && (*argument<='9'))
                    {
                        seed *= 10;
                        seed += *argument - '0';
                        argument++;
                    }
                    break;
                case 't':
                    argument++;
                    testNb=0;
                    while ((*argument>='0') && (*argument<='9'))
                    {
                        testNb *= 10;
                        testNb += *argument - '0';
                        argument++;
                    }
                    break;
                case 'P':   /* compressibility % */
                    argument++;
                    proba=0;
                    while ((*argument>='0') && (*argument<='9'))
                    {
                        proba *= 10;
                        proba += *argument - '0';
                        argument++;
                    }
                    if (proba<0) proba=0;
                    if (proba>100) proba=100;
                    break;
                default:
                    ;
                    return FUZ_usage();
                }
            }
        }
    }

    /* Get Seed */
    printf("Starting lz4sg tester (%i-bits, %s)\n", (int)(sizeof(size_t)*8), LZ4_VERSION);

    if (!seedset) seed = FUZ_GetMilliStart() % 10000;
    printf("Seed = %u\n", seed);
    if (proba!=FUZ_COMPRESSIBILITY_DEFAULT) printf("Compressibility : %i%%\n", proba);
    DISPLAYLEVEL(1, "Seed = %u Compressibility = %i%%\n", seed, proba);

    if (nbTests<=0) nbTests=1;

    if (testNb==0) result = basicTests(seed, ((double)proba) / 100);
    if (result) return 1;
    return fuzzerTests(seed, nbTests, testNb, ((double)proba) / 100, duration);
}

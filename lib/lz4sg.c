/* TODO: license and stuff */

/* LZ4SG is a stand-alone API to create LZ4-compressed frames from Scatter-Gather Lists.
 * conformant with LZ4F specification v1.5.1.
 * All related operations, including memory management, are handled internally by the library.
 * You need neither lz4.h nor lz4frame.h when using lz4sg.h.
 * */

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


/**************************************
*  Includes
**************************************/
#include "lz4sg.h"
#include "lz4.h"
//#include "lz4hc.h"
#include "xxhash.h"
#include "lz4frame.h"

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
#define ALLOCATOR(s)   calloc(1,s)
#define FREEMEM        free
#include <string.h>   /* memset, memcpy, memmove */
#define MEM_INIT       memset

/**************************************
*  Basic Types
**************************************/
#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L)   /* C99 */
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
*  constants
**************************************/
#define KB *(1<<10)
#define MB *(1<<20)
#define GB *(1<<30)

#define _1BIT  0x01
#define _2BITS 0x03
#define _3BITS 0x07
#define _4BITS 0x0F
#define _8BITS 0xFF

#define LZ4F_MAGICNUMBER 0x184D2204U

static const size_t frameHeaderSize = 15;
static const size_t BHSize = 4;
static const size_t frameEndSize = 4;

/**************************************
*  Helper routines
**************************************/

/* unoptimized version; solves endianess & alignment issues */
static U32 LZ4_SG_readLE32 (const BYTE* srcPtr)
{
    U32 value32 = srcPtr[0];
    value32 += (srcPtr[1]<<8);
    value32 += (srcPtr[2]<<16);
    value32 += ((U32)srcPtr[3])<<24;
    return value32;
}

static void LZ4_SG_writeLE32 (BYTE* dstPtr, U32 value32)
{
    dstPtr[0] = (BYTE)value32;
    dstPtr[1] = (BYTE)(value32 >> 8);
    dstPtr[2] = (BYTE)(value32 >> 16);
    dstPtr[3] = (BYTE)(value32 >> 24);
}

static U64 LZ4_SG_readLE64 (const BYTE* srcPtr)
{
    U64 value64 = srcPtr[0];
    value64 += ((U64)srcPtr[1]<<8);
    value64 += ((U64)srcPtr[2]<<16);
    value64 += ((U64)srcPtr[3]<<24);
    value64 += ((U64)srcPtr[4]<<32);
    value64 += ((U64)srcPtr[5]<<40);
    value64 += ((U64)srcPtr[6]<<48);
    value64 += ((U64)srcPtr[7]<<56);
    return value64;
}

static void LZ4_SG_writeLE64 (BYTE* dstPtr, U64 value64)
{
    dstPtr[0] = (BYTE)value64;
    dstPtr[1] = (BYTE)(value64 >> 8);
    dstPtr[2] = (BYTE)(value64 >> 16);
    dstPtr[3] = (BYTE)(value64 >> 24);
    dstPtr[4] = (BYTE)(value64 >> 32);
    dstPtr[5] = (BYTE)(value64 >> 40);
    dstPtr[6] = (BYTE)(value64 >> 48);
    dstPtr[7] = (BYTE)(value64 >> 56);
}


static BYTE LZ4_SG_headerChecksum (const void* header, size_t length)
{
    U32 xxh = XXH32(header, length, 0);
    return (BYTE)(xxh >> 8);
}

static size_t LZ4_SG_compressBegin(void *dstBuffer, size_t contentSize, size_t max_block_size)
{
    BYTE* const dstStart = (BYTE*)dstBuffer;
    BYTE* dstPtr = dstStart;
    BYTE* headerStart;

    /* Magic Number */
    LZ4_SG_writeLE32(dstPtr, LZ4F_MAGICNUMBER);
    dstPtr += 4;
    headerStart = dstPtr;

    /* FLG Byte */
    *dstPtr++ = (BYTE)(((1 & _2BITS)         << 6)    /* Version('01') */
        + ((LZ4F_blockLinked & _1BIT )       << 5)    /* Block mode */
        + ((LZ4F_noContentChecksum & _1BIT ) << 2)    /* Frame checksum */
        + ((contentSize > 0)                 << 3));  /* Frame content size */
    /* BD Byte */
    //TODO: fine-grained selection
    BYTE block_size_id = (max_block_size <= 64 KB) ? LZ4F_max64KB : LZ4F_max4MB;
    *dstPtr++ = (BYTE)((block_size_id & _3BITS) << 4); /* Max block size */
    /* Optional Frame content size field */
    if (contentSize > 0)
    {
        LZ4_SG_writeLE64(dstPtr, contentSize);
        dstPtr += 8;
    }
    /* CRC Byte */
    *dstPtr = LZ4_SG_headerChecksum(headerStart, dstPtr - headerStart);
    dstPtr++;

    return (dstPtr - dstStart);
}
static int LZ4_SG_decodeHeader(const void *srcVoidPtr, U64 *contentSize)
{
    BYTE FLG, HC;
    unsigned version, blockMode, blockChecksumFlag, contentSizeFlag, contentChecksumFlag;
//    BYTE BD;
//    unsigned blockSizeID;
    const BYTE* srcPtr = (const BYTE*)srcVoidPtr;

    *contentSize = 0;

    /* Magic Number */
    U32 magic = LZ4_SG_readLE32(srcPtr);
    if (unlikely(magic != LZ4F_MAGICNUMBER)) {
        DBGLVL(1, "Invalid magic %08x", magic);
        return -1;
    }

    /* Flags */
    FLG = srcPtr[4];
    version = (FLG>>6) & _2BITS;
    blockMode = (FLG>>5) & _1BIT;
    blockChecksumFlag = (FLG>>4) & _1BIT;
    contentSizeFlag = (FLG>>3) & _1BIT;
    contentChecksumFlag = (FLG>>2) & _1BIT;

    /* Frame Header Size */
    //frameHeaderSize = contentSizeFlag ? maxFHSize : minFHSize;

    if (unlikely(version != 1)) {
        DBGLVL(1, "Unsupported version %d", version);
        return -2;
    }
    /* checksum */
    HC = LZ4_SG_headerChecksum(srcPtr+4, frameHeaderSize-5);
    if (unlikely(HC != srcPtr[frameHeaderSize-1])) {
        DBGLVL(1, "Header checksum mismatch");
        return -3;
    }

    if (unlikely(blockChecksumFlag != 0)) {
        DBGLVL(1, "Block checksum unsupported");
        return -4;
    }

    if (unlikely(contentChecksumFlag != LZ4F_noContentChecksum)) {
        DBGLVL(1, "Content checksum unsupported");
        return -5;
    }

    // required by SG (for now)
    if (unlikely(contentSizeFlag != 1)) {
        DBGLVL(1, "Original Content-size is required");
        return -6;
    }
    *contentSize = LZ4_SG_readLE64(srcPtr+6);

    if (unlikely(blockMode != LZ4F_blockLinked)) {
        DBGLVL(1, "Independent Blocks unsupported (for now)");
        return -7;
    }

    // valid header
    return frameHeaderSize;
}

static int LZ4_SG_compressEnd(
        const LZ4SG_in_t * sg_in , size_t sg_in_len,
        const LZ4SG_out_t* sg_out, size_t sg_out_len,
        size_t max_out_block_size,
        size_t out_position, size_t contentSizeOriginal, size_t contentSizeActual, size_t maxOutputSize)
{
    if (sg_out_len <= 0) return -2;

    /* endMark */

    // check room for end mark
    if (out_position + frameEndSize > maxOutputSize) {
        return 0;
    }

    // seek to out position
    const LZ4SG_out_t *sgo = sg_out;
    const LZ4SG_out_t *sgoend = sg_out + sg_out_len;
    void *endMarkPtr = NULL;
    int pos = 0;
    while (sgo != sgoend) {
        size_t next_pos = pos + sgo->sg_len;
        if (next_pos > out_position) {
            endMarkPtr = (BYTE *)sgo->sg_base + out_position - pos;
            break;
        }
        else {
            pos = next_pos;
            sgo++;
        }
    }

    // if output buffers consumed without reaching output position
    if (endMarkPtr == NULL) {
        // no room for frame end mark
        return 0;
    }

    // check overlap of end mark
    // can we write end-mark directly? or should we split it with next buffer?
    int cur_buf_rem = (BYTE *)sgo->sg_base + sgo->sg_len - (BYTE *)endMarkPtr;
    if ((int)frameEndSize <= cur_buf_rem) {
        memset(endMarkPtr, 0, frameEndSize);
    }
    else {
        // can we split? is there enough room?
        sgo++;
        if (sgo == sgoend) {
            // not enough room
            return 0;
        }
        int next_buf_rem = frameEndSize - cur_buf_rem;
        memset(endMarkPtr, 0, cur_buf_rem);
        memset(sgo->sg_base, 0, next_buf_rem);
    }

    /* header overwrite */

    // search for maximum input block size
    // max_in_block_size is an over-estimation, but that's usually fine
    // (would theoretically lead to larger memory allocation than required)
    size_t max_in_block_size = 0;
    const LZ4SG_in_t *sgi = sg_in;
    const LZ4SG_in_t *sgiend = sg_in + sg_in_len;
    while (sgi != sgiend) {
        //NOTE: final buffer may be larger than actual usage, over-estimation is fine for now
        if (max_in_block_size < sgi->sg_len) max_in_block_size = sgi->sg_len;
        sgi++;
    }

    // fix max block size in frame header and content size, if necessary
    size_t max_block_size = (max_in_block_size > max_out_block_size) ? max_in_block_size : max_out_block_size;
    if ((max_block_size > 64*1024) || (contentSizeActual != contentSizeOriginal)) {
        // alternatively - pre-compute and use value in Begin
        (void)LZ4_SG_compressBegin(sg_out->sg_base, contentSizeActual, max_block_size);
    }

    return out_position + 4;
}

size_t LZ4_SG_compressBound(size_t sourceSize, size_t sg_in_len, size_t sg_out_len)
{
    //even though it may work - these are not the expected use cases and can be avoided
    if (unlikely(sourceSize > LZ4_MAX_INPUT_SIZE)) return 0;
    if (unlikely(sourceSize <= sg_in_len)) return 0;

    //NOTE:
    // Exact calculation (requires input SGL):
    // frame-header + frame-end + (number of input blocks + number of output blocks) * 4 +
    //             zero-padding-blocks * (4+1) +
    //             lz4_bound(input_block_length1) + ... + lz4_bound(input_block_lengthN)
    //
    // where
    //    zero-padding-blocks == min(number of input blocks, number of output blocks)
    //
    //  The following approximation is simpler, and probably good enough (for expected use
    // cases, i.e., non-trivial buffers)
    size_t one_buffer_length = LZ4_compressBound(sourceSize / sg_in_len);
    size_t min_buffer_list_len = (sg_in_len > sg_out_len) ? sg_out_len : sg_in_len;
    size_t patch_too_few_zero_paddings = 1;
    size_t zero_padding_blocks = (patch_too_few_zero_paddings + min_buffer_list_len) * (1+BHSize);
    size_t patch_too_small_bug = 13 + (sg_in_len == 1 ? 100 : 0);
    return
            frameHeaderSize + frameEndSize + patch_too_small_bug +
            (sg_in_len + sg_out_len) * BHSize +
            zero_padding_blocks +
            one_buffer_length * sg_in_len;
}

/**************************************
*  Compression internal routines
**************************************/



static int LZ4_compress_fast_sg_extState (
        void* state,
        const LZ4SG_in_t * sg_in , int sg_in_len ,
        const LZ4SG_out_t* sg_out, int sg_out_len,
        size_t out_skip_size,
        size_t* sourceSizePtr,
        size_t* maxOutBlockSizePtr,
        size_t maxDestSize,
        int acceleration)
{
    const int sourceSize = *sourceSizePtr;
    *sourceSizePtr = 0;

    const LZ4SG_in_t  *sgiend = sg_in  + sg_in_len ;
    const LZ4SG_out_t *sgoend = sg_out + sg_out_len;

    //TODO: verify arguments
    // input assumptions (verify?):
    // sourceSize  is equal to or smaller than the sum of  input buffer lengths
    // maxDestSize is equal to or smaller than the sum of output buffer lengths
    if (sg_in_len  <= 0) return -1;
    if (sg_out_len <= 0) return -2;

    for (const LZ4SG_in_t  *sgi_ = sg_in ; sgi_ != sgiend; sgi_++)
        if (unlikely((sgi_->sg_len < 1) || (sgi_->sg_len > 4 MB))) {
            DBGLVL(1, "%s: Unsupported input buffer %i length %i\n", __FUNCTION__, (int)(sgi_-sg_in), (int)sgi_->sg_len);
            return -3;
        }
    for (const LZ4SG_out_t *sgo_ = sg_out; sgo_ != sgoend; sgo_++)
        if (unlikely(sgo_->sg_len < 10)) {
            DBGLVL(1, "%s: Unsupported output buffer %i length %i\n", __FUNCTION__, (int)(sgo_-sg_out), (int)sgo_->sg_len);
            return -4;
        }

    int max_out_block_size = 0;
    int ipos = 0;
    int opos = out_skip_size;
    int total_processed_input  = ipos;
    int total_processed_output = opos;

    const LZ4SG_in_t  *sgi = sg_in ;
    const LZ4SG_out_t *sgo = sg_out;

    while ((total_processed_input < sourceSize) && ((total_processed_output+BHSize) < maxDestSize)) {
        // mark and skip block size position
        const char* source = ((const char*)sgi->sg_base) + ipos;
              char*   dest = ((char*)sgo->sg_base) + opos;
        void *oBS = dest;

        dest += BHSize;
        opos += BHSize;
        total_processed_output += BHSize;

        const int irem =  sourceSize - total_processed_input ;
        const int orem = maxDestSize - total_processed_output;
        int iSize = sgi->sg_len - ipos;
        int oSize = sgo->sg_len - opos;
        if (unlikely(irem < iSize)) iSize = irem;
        if (unlikely(orem < oSize)) oSize = orem;

        if ((sg_in_len > 1) || (sg_out_len > 1)) {
            DBGLVL(1, "\nI_%d,%d\t"
                    "O_%d,%d\t"
                    "l_%d,%d\t"
                    "p_%d,%d\t"
                    "r_%d,%d\t"
                    "S%d,%d\n",
                    sourceSize, total_processed_input,
                    maxDestSize, total_processed_output,
                    sg_in_len, sg_out_len,
                    ipos, opos,
                    irem, orem,
                    iSize, oSize);
        }

        int iSize_ = iSize;
        int oSize_ = LZ4_compress_fast_destSize_continue((LZ4_stream_t*)state, source, dest, &iSize_, oSize, acceleration);

        if ((sg_in_len > 1) || (sg_out_len > 1)) {
            DBGLVL(1, "\nO source %d dest %d\n", iSize_, oSize_);
        }

        // if error - abort
        if ((oSize_ < 0) || (iSize_ == 0)) {
            DBGLVL(1, "\n%s ERROR : iSize_ %d oSize_ %d\n",
                    __FUNCTION__, iSize_, oSize_);
            return -total_processed_output + oSize_;
        }

        // write block size to pre-allocated position
        LZ4_SG_writeLE32(oBS, oSize_);
        if (oSize_ > max_out_block_size) max_out_block_size = oSize_;

        // advance total input and output
        total_processed_input  += iSize_;
        total_processed_output += oSize_;

        // input consumed?
        if (iSize_ == iSize) {
            sgi++;
            // total input consumed?
            if (unlikely(sgi == sgiend)) goto _end_cleanup;
            ipos = 0;
        }
        else {
            ipos += iSize_;
        }
        // output (almost) consumed?
        if ((oSize_+(int)(1+BHSize)) >= oSize) {
            sgo++;
            // total output consumed?
            if (unlikely(sgo == sgoend)) goto _end_cleanup;

            // if output has less than minimum required to ensure input will be advanced,
            // yet there is enough room for extra data in another buffer
            int cur_buf_rem = oSize-oSize_;
            if (unlikely((oSize_ != oSize) && ((total_processed_output+BHSize) < maxDestSize))) {
                int next_buf_rem = 1+BHSize - cur_buf_rem;
                // pad final output bytes with zero block (NOTE: is this compatible with non-sg decompress?)
                DBGLVL(1, "\n0pad T %d %d B %d %d cur_rem %d next_rem %d\n",
                        total_processed_input, total_processed_output, iSize_, oSize_, cur_buf_rem, next_buf_rem);
                BYTE zero_pad_block[1+BHSize];
                LZ4_SG_writeLE32(zero_pad_block, 1);
                zero_pad_block[BHSize] = 0;

                memcpy(dest+oSize_, zero_pad_block, cur_buf_rem);
                memcpy(sgo->sg_base, &zero_pad_block[cur_buf_rem], next_buf_rem);

                opos = next_buf_rem;
                total_processed_output += 1+BHSize;
            }
            else {
                opos = 0;
            }
        }
        else {
            opos += oSize_;
        }
    }

_end_cleanup:
    if ((sg_in_len > 1) || (sg_out_len > 1)) {
        DBGLVL(1, "%s total_processed_input %d total_processed_output %d\n", __FUNCTION__, total_processed_input, total_processed_output);
    }

    *sourceSizePtr = total_processed_input;
    *maxOutBlockSizePtr = max_out_block_size;

    return total_processed_output;
}

static int LZ4_decompress_fast_sg_extState (
        void* state,
        const LZ4SG_in_t * sg_in , int sg_in_len ,
        const LZ4SG_out_t* sg_out, int sg_out_len,
        size_t in_skip_size,
        int compressedSize,
        int originalSize)
{
    const U32 max_supported_block_size = 4 MB;
    U32 compressed_block_size;
    const LZ4SG_in_t  *sgiend = sg_in  + sg_in_len ;
    const LZ4SG_out_t *sgoend = sg_out + sg_out_len;

    //TODO: verify arguments
    // input assumptions (verify?):
    // sourceSize  is equal to or smaller than the sum of  input buffer lengths
    // maxDestSize is equal to or smaller than the sum of output buffer lengths
    if (sg_in_len  <= 0) return -1;
    if (sg_out_len <= 0) return -2;

    for (const LZ4SG_in_t  *sgi_ = sg_in ; sgi_ != sgiend; sgi_++)
        if (unlikely(sgi_->sg_len < 2)) return -3;
    for (const LZ4SG_out_t *sgo_ = sg_out; sgo_ != sgoend; sgo_++)
        if (unlikely(sgo_->sg_len < 1)) return -4;

    int ipos = in_skip_size;
    int opos = 0;
    int total_processed_input  = ipos;
    int total_processed_output = opos;

    const LZ4SG_in_t  *sgi = sg_in ;
    const LZ4SG_out_t *sgo = sg_out;

    while (((total_processed_input+(int)BHSize) < compressedSize) && (total_processed_output < originalSize)) {
        const char* source = ((const char*)sgi->sg_base) + ipos;
              char*   dest = ((char*)sgo->sg_base) + opos;
        int irem;
        int orem;

        compressed_block_size = LZ4_SG_readLE32((const BYTE *)source);
        if (unlikely(compressed_block_size > max_supported_block_size)) {
            DBGLVL(1, "\n%s ERROR : Unsupported compressed block size %d",
                    __FUNCTION__, compressed_block_size);
            return -compressed_block_size;
        }

        source += BHSize;
        ipos += BHSize;
        total_processed_input += BHSize;

_next_compressed_block:

        irem = compressedSize - total_processed_input;
        orem =   originalSize - total_processed_output;
        int iSize = sgi->sg_len - ipos;
        int oSize = sgo->sg_len - opos;
        if (unlikely(irem < iSize)) iSize = irem;
        if (unlikely(orem < oSize)) oSize = orem;

        if ((sg_in_len > 1) || (sg_out_len > 1)) {
            DBGLVL(1, "\nI_%d,%d O_%d,%d l_%d,%d p_%d,%d r_%d,%d S%d,%d\n",
                    compressedSize, total_processed_input, originalSize, total_processed_output,
                    sg_in_len, sg_out_len, ipos, opos, irem, orem, iSize, oSize);
        }

        if (unlikely((int)compressed_block_size > iSize)) {
            DBGLVL(1, "\n%s ERROR : Incompatible compressed block size %d (larger than iSize %d)",
                    __FUNCTION__, compressed_block_size, iSize);
            return -compressed_block_size;
        }

        //NOTE: for now, assume output buffer is large enough and fail if it isn't
        //TODO: generalize with a destSize variant - is it possible to resume or should we decompress
        // entire block from scratch with a larger buffer?
        int iSize_ = compressed_block_size;
        int result = LZ4_decompress_safe_continue((LZ4_streamDecode_t*)state, source, dest, iSize_, oSize);
        int oSize_ = result;

        if ((sg_in_len > 1) || (sg_out_len > 1)) {
            DBGLVL(1, "\nO result %d\n", result);
        }

        // if error - abort
        if (result < 0) {
            DBGLVL(1, "\n%s ERROR : result %d",
                    __FUNCTION__, result);
            return -total_processed_output - compressed_block_size;
        }

        // advance total input and output
        total_processed_input  += iSize_;
        total_processed_output += oSize_;

        // output consumed?
        if (oSize_ == oSize) {
            sgo++;
            // total output consumed?
            if (unlikely(sgo == sgoend)) goto _dend_cleanup;
            opos = 0;
        }
        else {
            opos += oSize_;
        }
        // input (almost) consumed?
        if ((iSize_+(int)(1+BHSize)) >= iSize) {
            sgi++;
            // total input consumed?
            if (unlikely(sgi == sgiend)) goto _dend_cleanup;

            // if input has less than minimum required to ensure output will be advanced,
            // yet there is enough room for extra data in another buffer
            int cur_buf_rem = iSize-iSize_;
            if (unlikely((iSize_ != iSize) && ((total_processed_input+(int)BHSize) < compressedSize))) {
                int next_buf_rem = 1+BHSize - cur_buf_rem;

                BYTE zero_pad_block[1+BHSize];

                memcpy(zero_pad_block, source+iSize_, cur_buf_rem);
                memcpy(&zero_pad_block[cur_buf_rem], sgi->sg_base, next_buf_rem);

                compressed_block_size = LZ4_SG_readLE32(zero_pad_block);

                ipos = next_buf_rem;
                total_processed_input += 1+BHSize;

                DBGLVL(1, "\n0pad T %d %d ipos %d B %d %d compressed_block_size %d pad_value %02x\n",
                        total_processed_input, total_processed_output, ipos,
                        iSize_, oSize_, compressed_block_size, zero_pad_block[BHSize]);
                if (compressed_block_size == 1) {
                    if (zero_pad_block[BHSize] == 0) {
                        // valid pad block
                        //TODO: ???

                        // skip zero-pad block (verify zero) (NOTE: is this compatible with non-sg decompress?)
                    }
                    else {
                        // invalid pad token
                        return -total_processed_input;
                    }
                }
                else {
                    // non-zero-pad block - skip block size and resume decompression
                    ipos--;
                    total_processed_input--;

                    goto _next_compressed_block;
                }
            }
            else {
                ipos = 0;
            }
        }
        else {
            ipos += iSize_;
        }
    }

_dend_cleanup:
    return total_processed_output;
}

/**************************************
*  Compression external routines
**************************************/

int LZ4_SG_compress (
        const LZ4SG_in_t * sg_in , size_t sg_in_len,
        const LZ4SG_out_t* sg_out, size_t sg_out_len,
        size_t* sourceSizePtr, size_t maxOutputSize, int acceleration)
{
    // minimal verification for frame header+BHSize (simplifies verification in LZ4_compress_fast_sg_extState)
    const size_t min_first_out_buf_len = frameHeaderSize+BHSize+2;
    const size_t contentSize = *sourceSizePtr;
    if (unlikely(sg_out->sg_len < min_first_out_buf_len)) {
        DBGLVL(1, "First output buffer too small (%i instead of minimum %i)\n", sg_out->sg_len, min_first_out_buf_len);
        *sourceSizePtr = 0;
        return 0;
    }

    // write frame header with content size
    int result = LZ4_SG_compressBegin(sg_out->sg_base, contentSize, 64 KB);
    if (unlikely(result < (int)frameHeaderSize)) {
        DBGLVL(1, "Unsupported arguments (%i)\n", result);
        *sourceSizePtr = 0;
        return 0;
    }

    size_t out_skip_size = result;
    size_t max_out_block_size = 0;

    // compress data
#if (HEAPMODE)
    void* ctx = ALLOCATOR(1, sizeof(LZ4_stream_t));   /* malloc-calloc always properly aligned */
#else
    LZ4_stream_t ctxBody;
    void* ctx = &ctxBody;
#endif
    LZ4_resetStream((LZ4_stream_t *)ctx);
    int result_position = LZ4_compress_fast_sg_extState(
            ctx,
            sg_in , sg_in_len ,
            sg_out, sg_out_len,
            out_skip_size,
            sourceSizePtr,
            &max_out_block_size,
            maxOutputSize,
            acceleration);
#if (HEAPMODE)
    FREEMEM(ctx);
#endif

    // finalize frame
    if (result_position > 0) {
        // write frame end mark (and possibly overwrite begin mark and header)
        result_position = LZ4_SG_compressEnd(
                sg_in , sg_in_len ,
                sg_out, sg_out_len,
                max_out_block_size,
                result_position, contentSize, *sourceSizePtr, maxOutputSize);
        if (unlikely(result_position <= 0)) {
            DBGLVL(1, "Final buffer too small for end mark (bound %i)\n", LZ4_SG_compressBound(contentSize, sg_in_len, sg_out_len));
        }
        else {
            DBGLVL(1, "%s result_position %i contentSize %i actual content size %i\n", __FUNCTION__, result_position, (int)contentSize, (int)*sourceSizePtr);
        }
    }

    return result_position;
}

int LZ4_SG_decompress (
        const LZ4SG_in_t * sg_in , size_t sg_in_len,
        const LZ4SG_out_t* sg_out, size_t sg_out_len,
        size_t* sourceSizePtr, size_t maxOutputSize)
{
    // minimal verification for frame header
    if (unlikely(sg_in->sg_len < frameHeaderSize)) return 0;

    // read frame header with content size
    U64 originalSize = 0;
    int result = LZ4_SG_decodeHeader(sg_in->sg_base, &originalSize);
    if (result < 0) return result;
    if (unlikely(result != (int)frameHeaderSize)) return 0;

    if (maxOutputSize < originalSize) {
        DBGLVL(1, "Insufficient total buffer size %d", (int)maxOutputSize);
        return 0;
    }

    size_t compressedSize = *sourceSizePtr;
    size_t in_skip_size = result;

    // decompress data
#if (HEAPMODE)
    void* crx = LZ4_createStreamDecode();
#else
    LZ4_streamDecode_t ctxBody;
    void* ctx = &ctxBody;
#endif
    (void)LZ4_setStreamDecode((LZ4_streamDecode_t *)ctx, NULL, 0);
    result = LZ4_decompress_fast_sg_extState(
            ctx,
            sg_in , sg_in_len ,
            sg_out, sg_out_len,
            in_skip_size,
            compressedSize, originalSize);
#if (HEAPMODE)
    FREEMEM(ctx);
#endif

    //TODO: verify end mark (may overlap buffers!)

    return result;
}

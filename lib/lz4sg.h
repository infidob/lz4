/* TODO: license and stuff */

/* LZ4SG is a stand-alone API to create LZ4-compressed frames from Scatter-Gather Lists.
 * conformant with LZ4F specification v1.5.1.
 * All related operations, including memory management, are handled internally by the library.
 * You need neither lz4.h nor lz4frame.h when using lz4sg.h.
 * */

#pragma once

#if defined (__cplusplus)
extern "C" {
#endif

/**************************************
*  Includes
**************************************/
#include <stddef.h>   /* size_t */


/***********************************************
*  Scatter-Gather Lists
***********************************************/

typedef struct {
    const void *sg_base;
    size_t      sg_len;
} LZ4SG_in_t;
typedef struct {
    void * sg_base;
    size_t sg_len;
} LZ4SG_out_t;

/***********************************************
*  Scatter-Gather Lists Compression Functions
***********************************************/

/*
LZ4_SG_compress_bound() :
    Similar to LZ4F_compressFrameBound(), takes into account LZ4F framing format with a separate block for each
    scatter-gather lists input/output pair, without content checksum.
    Result also includes overhead of embedding content size in frame header.
*/
size_t LZ4_SG_compressBound(size_t sourceSize, size_t sg_in_len, size_t sg_out_len);

/*
LZ4_SG_compress() :
    Similar to LZ4F_compressFrame(), uses scatter-gather lists as input and output buffers,
    with block dependency.

    Input is repeatedly processed 1 input/output pair at a time, until either is consumed.
    When an input buffer is consumed, next input buffer is used, using the previous buffer as
    external dictionary for the next phase. When an output buffer is consumed, next output buffer
    is used, using the previous buffer as prefix dictionary for the next phase.

    At any case, at most *sourceSizePtr bytes of input (content size) are read and at most maxOutputSize
    bytes of output are written.

    This function expects output to suffice for compression of entire data into a single frame.
    Content size (*sourceSizePtr) is embedded in the Frame Descriptor.
    Compression may fail if output buffers are too short.

        return : the number of bytes written into output buffers (necessarily <= maxOutputSize)
              or 0 if compression fails.
        *sourceSizePtr : will be updated to indicate how many bytes where read from input buffers
              to fill output buffers. New value is necessarily <= old value.

    Note: This function never writes outside of output buffers, and never reads outside of input buffers.
 */
int LZ4_SG_compress (
        const LZ4SG_in_t * sg_in , size_t sg_in_len,
        const LZ4SG_out_t* sg_out, size_t sg_out_len,
        size_t* sourceSizePtr, size_t maxOutputSize, int acceleration);

/*
LZ4_SG_decompress() :
    Similar to LZ4F_decompress(), uses scatter-gather lists as input and output buffers.

    Input is repeatedly processed 1 input/output pair at a time, until either is consumed.
    When input is consumed, next input buffer is used, using the previous output buffer as prefix dictionary
    for the next phase. When output is consumed, next output buffer is used, using the previous output buffer
    as external dictionary for the next phase.

    At any case, at most *sourceSizePtr bytes of input are read and at most maxOutputSize bytes of output are
    written.

    This function expects output to suffice for decompression of entire frame. If original content size is
    embedded in the Frame Descriptor, it is used to verify against maxOutputSize. Otherwise, decompression
    begins and may fail if output buffers are too short to decompress entire frame.

        return : the number of bytes written into output buffers (necessarily <= maxOutputSize)
              or a negative error code if decompression fails.
        *sourceSizePtr : will be updated to indicate how many bytes where read from input buffers
              to fill output buffers. New value is necessarily <= old value.

       Note: This function never writes outside of output buffers, and never reads outside of input buffers.
            Even though it is protected against malicious data packets, decompression is not guaranteed to
            succeed for any scatter-gather lists. If input was compressed with LZ4_SG_compress() and output
            corresponds to the same scatter-gather list given as input to LZ4_SG_compress(), success is
            guaranteed (unless input is malformed or corrupted).
 */
int LZ4_SG_decompress (
        const LZ4SG_in_t * sg_in , size_t sg_in_len,
        const LZ4SG_out_t* sg_out, size_t sg_out_len,
        size_t* sourceSizePtr, size_t maxOutputSize);



#if defined (__cplusplus)
}
#endif



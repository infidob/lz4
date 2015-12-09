LZ4_SG - Scatter Gather LZ4 
================================

LZ4_SG is a scatter-gather variant of the fast compression library LZ4 (by Cyan).
LZ4_SG links consecutive blocks into a single LZ4F frame compatible with LZ4F 
(v.1.5.0, i.e., can be decoded by LZ4F_decompress). LZ4_SG avoids copying 
memory of incompressible blocks, which are stored literally in place.

LZ4_SG library is provided as open-source software using BSD license.

Benchmarks
-------------------------

The benchmark program used is based on density's benchmark (by gpnuma
[Density]), which was extended to support scatter-gather compressors 
(TODO: fork it and push updates).
In order to evaluate SG, input buffer is first copied to 4KB buffers. 
4KB Output buffers are also allocated to accomodate compressed data.
Decompression uses compressed buffers as input and stores output in the 4KB
buffers used as input for compression.
The benchmark program was compiled with GCC 4.8.3 -O3 on centos with kernel 
2.6.32, and ran on reference system Intel(R) Xeon(R) CPU E5-2697 v3 @ 2.60GHz.
Benchmark evaluates the compression of reference [Silesia Corpus]
in single-thread mode.

|  Compressor          | Ratio   | Compression | Decompression |
|  ----------          | -----   | ----------- | ------------- |
|  LZ4                 | 47.64%  |  502 MB/s   |   2297 MB/s   |
|  LZ4'                | 47.64%  |  481 MB/s   |   2011 MB/s   |
|  LZ4+dual_memcpy     | 47.64%  |  470 MB/s   |   1863 MB/s   |
|**LZ4_SG**            | 49.01%  |  440 MB/s   |   2013 MB/s   |

dual_memcpy reference point corresponds to copying scattered input blocks to a
contiguous buffer, applying compression to another contiguous buffer, and then
copying results to scattered output blocks.
LZ4' is the modified version of LZ4 that supports SG, but is applied to
contiguous input and output buffers.

Multi-core benchmark results (8 concurrent runs in terminal):

|  Compressor          | Ratio   | Compression | Decompression |
|  ----------          | -----   | ----------- | ------------- |
|  LZ4'                | 47.64%  |  480 MB/s   |   1700 MB/s   |
|  LZ4+dual_memcpy     | 47.64%  |  470 MB/s   |   1490 MB/s   |
|**LZ4_SG**            | 49.01%  |  440 MB/s   |   1550 MB/s   |


Limitations
-------------------------
Decompress buffers should be same size as original buffers containing plain-text.
That is, buffer lengths used as source,target for compression can be safely used 
as target,source for decompression.

Known Issues
-------------------------
LZ4_SG_compressBound (used to verify sufficient memory for output before 
compression) is not tight with extremely small buffers lengths (e.g., 16 bytes). 

Documentation
-------------------------

See lib/lz4sg.h for usage details (basically, similar to vectored I/O iovec).

[Silesia Corpus]: http://sun.aei.polsl.pl/~sdeor/index.php?page=silesia
[LZ4 Homepage]: http://www.lz4.org
[Density]: https://github.com/centaurean/density

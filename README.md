# kirbyLzRle. 
Mixed LZ-RLE compression tool for HAL Laboratory games.


Synopsis:
```
kirbyLzRle [-d | -c] inFileName outFileName
```
  
Description:

***kirbyLzRle -d*** <inFile> <offset> <outFile>  Decompress block from given ROM file.

***kirbyLzRle -c*** <inFile> <outFile> Compress given plain block.

***-h, --help*** - Display help

***-v, --version*** - Output version information

See additional files in [release](https://github.com/romhack/kirbyLzRle/releases/latest) archive. 
  
This tool is written significantly later, than Revenant's [exhal/inhal](https://github.com/devinacker/exhal/) with only two purposes: 

1. Show that such relatively complex compression tools can be elegantly written in Haskell (300 lines for compression and decompression, including parsing command line parameters)
2. Write a more spatially efficient packer than inhal (which was also more efficient than the original packer from the HAL laboratory)

The tool was tested on the NES game "Kirby's Adventure", but given the format, it should work on the same games as exhal / inhal.

Build with [Haskell Stack](https://haskellstack.org) tool.

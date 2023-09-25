## LuaJIT Decompiler v2

The goal of this project is to replace the old and now mostly defunct python decompiler  
with a new and completely rewritten version which fixes all of the bugs and quirks the python decompiler had  
while also offering full support for gotos and stripped bytecode including upvalues.

## Usage

**WARNING: The current version is incomplete and may not work!**  

1. Head to the release section and download the latest executable.
2. Drag and drop a valid LuaJIT bytecode file onto the exe.
3. If the program succeeds, then a `.lua` file containing the decompiled lua code  
will be placed in the `output` folder located in the same directory as the exe.

## TODO

Missing features from python decompiler:
* recursive processing of files in a directory

Other missing features:
* multi assignments
* improved number formatting
* auto generated locals and do blocks for stripped bytecode
* bytecode big endian support
* improved decompilation logic for conditional assignments and if statements
* multi threading for faster recursive file processing

------------

This project uses an boolean expression decompilation algorithm which is based on this paper:  
[www.cse.iitd.ac.in/~sak/reports/isec2016-paper.pdf](https://www.cse.iitd.ac.in/~sak/reports/isec2016-paper.pdf)

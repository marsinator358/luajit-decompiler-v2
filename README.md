## LuaJIT Decompiler v2

The goal of this project is to replace the old and now mostly defunct python decompiler  
with a new and completely rewritten version which fixes all of the bugs and quirks the python decompiler had  
while also offering full support for gotos and stripped bytecode including upvalues.

## Usage

1. Head to the release section and download the latest executable.
2. Drag and drop a valid LuaJIT bytecode file or a folder containing such files onto the exe.
3. All successfully decompiled `.lua` files are placed into the `output` folder  
located in the same directory as the exe.

Feel free to [report any issues](https://github.com/marsinator358/luajit-decompiler-v2/issues/new) you have.

## TODO

* auto generated locals and do blocks for stripped bytecode
* bytecode big endian support
* improved decompilation logic for conditional assignments and if statements
* ~multi threading for faster recursive file processing~  
not needed, should already be fast enough

---

This project uses an boolean expression decompilation algorithm which is based on this paper:  
[www.cse.iitd.ac.in/~sak/reports/isec2016-paper.pdf](https://www.cse.iitd.ac.in/~sak/reports/isec2016-paper.pdf)

## LuaJIT Decompiler v2

*LuaJIT Decompiler v2* is a replacement tool for the old and now mostly defunct python decompiler.  
The project fixes all of the bugs and quirks the python decompiler had while also offering  
full support for gotos and stripped bytecode including locals and upvalues.

## Usage

1. Head to the release section and download the latest executable.
2. Drag and drop a valid LuaJIT bytecode file or a folder containing such files onto the exe.  
Alternatively, run the program in a command prompt. Use `-?` to show usage and options.
3. All successfully decompiled `.lua` files are placed by default into the `output` folder  
located in the same directory as the exe.

Feel free to [report any issues](https://github.com/marsinator358/luajit-decompiler-v2/issues/new) you have.

## TODO

* bytecode big endian support
* improved decompilation logic for conditional assignments

---

This project uses an boolean expression decompilation algorithm that is based on this paper:  
[www.cse.iitd.ac.in/~sak/reports/isec2016-paper.pdf](https://www.cse.iitd.ac.in/~sak/reports/isec2016-paper.pdf)

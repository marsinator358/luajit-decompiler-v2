**WORK IN PROGRESS**

### LuaJIT Decompiler v2

------------

The goal of this project is to replace the old and now mostly defunct python decompiler  
with a modern version which fixes all of the bugs and quirks the python decompiler had  
while also offering full support for gotos and stripped bytecode including upvalues.

### Usage

------------

**WARNING: The current release version is for debugging purposes only!**  
The output lua code may contain lots of gotos and auto generated variables.  
Loop conditions, else statements and multi assignments are currently not processed.

1. Head to the release section and download the latest exe.
2. Drag and drop a valid LuaJIT bytecode file onto the exe.
3. If the program succeeds, then a `.lua` file containing the output lua code  
will be placed in the `output` folder located in the same directory as the exe.

------------

This project uses an boolean expression decompilation algorithm which is based on this paper:  
[www.cse.iitd.ac.in/~sak/reports/isec2016-paper.pdf](https://www.cse.iitd.ac.in/~sak/reports/isec2016-paper.pdf)

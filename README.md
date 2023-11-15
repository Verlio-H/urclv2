# urclv2
A proof of concept compiler to convert the wip urclv2 into c

Compile using gfortran, ifort, or any other f2003 compatible fortran compiler.

Pass the input file name as an argument and pass the output file as -o argument, otherwise input.urcl, and output.c will be used by default

The produced C code is not standard compliant but seems to work in gcc and clang at least on x86-64 (does not work in msvc).

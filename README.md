Compiler for my URCLv2 proposal

Compile with any modern fortran compiler (gfortran, ifort, etc.)

Currently has both a 16 bit urcl backend (iris version of urcl) and a c backend (use -c, not standard c but works in gcc and clang)
For the iris backend you have to specify -urcx if you want to run it in urcx
Specify output file with -o

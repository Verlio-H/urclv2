#!/bin/bash
./compiler $1 -arch C
echo "compiled to c"
gcc output.c include/*.c -I/usr/local/include -lglfw -framework OpenGL -Ofast -march=native
echo "running"
./a.out
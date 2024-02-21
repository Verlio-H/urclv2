#!/bin/bash
./compiler $1 -arch C
gcc-13 output.c include/*.c -I/usr/local/include -lglfw -framework OpenGL -Ofast -march=native
./a.out
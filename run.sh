./compiler $1 -arch C
gcc output.c include/*.c -I/usr/local/include -lglfw -framework OpenGL -Ofast
./a.out
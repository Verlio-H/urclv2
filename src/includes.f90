module includes
    character(len=*), parameter :: print32 = &
     '.print32'//achar(10)//&
     'HPSH R3'//achar(10)//&
     'HPSH R4'//achar(10)//&
     'HPSH R5'//achar(10)//&
     'HPSH R6'//achar(10)//&
     'HPSH R7'//achar(10)//&
     'MOV R6 R0'//achar(10)//&
     'MOV R3 R0'//achar(10)//&
     'MOV R4 R0'//achar(10)//&
     'MOV R5 R0'//achar(10)//&
     '.loop'//achar(10)//&
     'AND R7 R5 0xF0'//achar(10)//&
     'BRL ~+2 R7 0x50'//achar(10)//&
     'ADD R5 R5 0x30'//achar(10)//&
     'AND R7 R5 0xF'//achar(10)//&
     'BRL ~+2 R7 0x5'//achar(10)//&
     'ADD R5 R5 0x3'//achar(10)//&
     'AND R7 R4 0xF000'//achar(10)//&
     'BRL ~+2 R7 0x5000'//achar(10)//&
     'ADD R4 R4 0x3000'//achar(10)//&
     'AND R7 R4 0xF00'//achar(10)//&
     'BRL ~+2 R7 0x500'//achar(10)//&
     'ADD R4 R4 0x300'//achar(10)//&
     'AND R7 R4 0xF0'//achar(10)//&
     'BRL ~+2 R7 0x50'//achar(10)//&
     'ADD R4 R4 0x30'//achar(10)//&
     'AND R7 R4 0xF'//achar(10)//&
     'BRL ~+2 R7 0x5'//achar(10)//&
     'ADD R4 R4 0x3'//achar(10)//&
     'AND R7 R3 0xF000'//achar(10)//&
     'BRL ~+2 R7 0x5000'//achar(10)//&
     'ADD R3 R3 0x3000'//achar(10)//&
     'AND R7 R3 0xF00'//achar(10)//&
     'BRL ~+2 R7 0x500'//achar(10)//&
     'ADD R3 R3 0x300'//achar(10)//&
     'AND R7 R3 0xF0'//achar(10)//&
     'BRL ~+2 R7 0x50'//achar(10)//&
     'ADD R3 R3 0x30'//achar(10)//&
     'AND R7 R3 0xF'//achar(10)//&
     'BRL ~+2 R7 0x5'//achar(10)//&
     'ADD R3 R3 0x3'//achar(10)//&
     '//shift'//achar(10)//&
     'LSH R5 R5'//achar(10)//&
     'BNC ~+2 R4 R4'//achar(10)//&
     'INC R5 R5'//achar(10)//&
     'LSH R4 R4'//achar(10)//&
     'BNC ~+2 R3 R3'//achar(10)//&
     'INC R4 R4'//achar(10)//&
     'LSH R3 R3'//achar(10)//&
     'BNC ~+2 R2 R2'//achar(10)//&
     'INC R3 R3'//achar(10)//&
     'LSH R2 R2'//achar(10)//&
     'BNC ~+2 R1 R1'//achar(10)//&
     'INC R2 R2'//achar(10)//&
     'LSH R1 R1'//achar(10)//&
     'INC R6 R6'//achar(10)//&
     'BLE .loop R6 31'//achar(10)//&
     '.printbcd'//achar(10)//&
     'MOV R6 R0'//achar(10)//&
     'AND R7 R5 0xF0'//achar(10)//&
     'BRZ ~+4 R7'//achar(10)//&
     'IMM R6 -1'//achar(10)//&
     'BSR R7 R7 4'//achar(10)//&
     'OUT %NUMB R7'//achar(10)//&
     'AND R7 R5 0x0F'//achar(10)//&
     'BRE ~+3 R7 R6'//achar(10)//&
     'IMM R6 -1'//achar(10)//&
     'OUT %NUMB R7'//achar(10)//&
     'AND R7 R4 0xF000'//achar(10)//&
     'BRE ~+4 R7 R6'//achar(10)//&
     'IMM R6 -1'//achar(10)//&
     'BSR R7 R7 12'//achar(10)//&
     'OUT %NUMB R7'//achar(10)//&
     'AND R7 R4 0xF00'//achar(10)//&
     'BRE ~+4 R7 R6'//achar(10)//&
     'IMM R6 -1'//achar(10)//&
     'BSR R7 R7 8'//achar(10)//&
     'OUT %NUMB R7'//achar(10)//&
     'AND R7 R4 0xF0'//achar(10)//&
     'BRE ~+4 R7 R6'//achar(10)//&
     'IMM R6 -1'//achar(10)//&
     'BSR R7 R7 4'//achar(10)//&
     'OUT %NUMB R7'//achar(10)//&
     'AND R7 R4 0xF'//achar(10)//&
     'BRE ~+3 R7 R6'//achar(10)//&
     'IMM R6 -1'//achar(10)//&
     'OUT %NUMB R7'//achar(10)//&
     'AND R7 R3 0xF000'//achar(10)//&
     'BRE ~+4 R7 R6'//achar(10)//&
     'IMM R6 -1'//achar(10)//&
     'BSR R7 R7 12'//achar(10)//&
     'OUT %NUMB R7'//achar(10)//&
     'AND R7 R3 0xF00'//achar(10)//&
     'BRE ~+4 R7 R6'//achar(10)//&
     'IMM R6 -1'//achar(10)//&
     'BSR R7 R7 8'//achar(10)//&
     'OUT %NUMB R7'//achar(10)//&
     'AND R7 R3 0xF0'//achar(10)//&
     'BRE ~+3 R7 R6'//achar(10)//&
     'BSR R7 R7 4'//achar(10)//&
     'OUT %NUMB R7'//achar(10)//&
     'AND R7 R3 0xF'//achar(10)//&
     'OUT %NUMB R7'//achar(10)//&
     'HPOP R7'//achar(10)//&
     'HPOP R6'//achar(10)//&
     'HPOP R5'//achar(10)//&
     'HPOP R4'//achar(10)//&
     'HPOP R3'//achar(10)//&
     'HRET'
    character(len=*), parameter :: div32 = &
     '.div32'//achar(10)//&
     'BNZ ~+3 R7'//achar(10)//&
     'BNZ ~+2 R8'//achar(10)//&
     'HLT'//achar(10)//&
     'HPSH R9'//achar(10)//&
     'HPSH R10'//achar(10)//&
     'IMM R9 1'//achar(10)//&
     'MOV R10 R0'//achar(10)//&
     'MOV R3 R0'//achar(10)//&
     'MOV R4 R0'//achar(10)//&
     '.loop1'//achar(10)//&
     'LSH R8 R8'//achar(10)//&
     'BNC ~+2 R7 R7'//achar(10)//&
     'INC R8 R8'//achar(10)//&
     'LSH R7 R7'//achar(10)//&
     'LSH R10 R10'//achar(10)//&
     'BNC ~+2 R9 R9'//achar(10)//&
     'INC R10 R10'//achar(10)//&
     'LSH R9 R9'//achar(10)//&
     'BRL .loop1 R8 R6'//achar(10)//&
     'BNE ~+2 R8 R6'//achar(10)//&
     'BLE .loop1 R7 R5'//achar(10)//&
     'RSH R7 R7'//achar(10)//&
     'BEV ~+2 R8'//achar(10)//&
     'ADD R7 R7 @MSB'//achar(10)//&
     'RSH R8 R8'//achar(10)//&
     'RSH R9 R9'//achar(10)//&
     'BEV ~+2 R10'//achar(10)//&
     'ADD R9 R9 @MSB'//achar(10)//&
     'RSH R10 R10'//achar(10)//&
     '.loop2'//achar(10)//&
     'BRL .skip R6 R8'//achar(10)//&
     'BNE ~+2 R6 R8'//achar(10)//&
     'BRL .skip R5 R7'//achar(10)//&
     'NOT R5 R5'//achar(10)//&
     'NOT R6 R6'//achar(10)//&
     'ADD R25 R6 R8'//achar(10)//&
     'BNC ~+2 R5 R7'//achar(10)//&
     'INC R25 R25'//achar(10)//&
     'MOV R6 R25'//achar(10)//&
     'ADD R5 R5 R7'//achar(10)//&
     'NOT R5 R5'//achar(10)//&
     'NOT R6 R6'//achar(10)//&
     'OR R3 R3 R9'//achar(10)//&
     'OR R4 R4 R10'//achar(10)//&
     '.skip'//achar(10)//&
     'RSH R9 R9'//achar(10)//&
     'BEV ~+2 R10'//achar(10)//&
     'ADD R9 R9 @MSB'//achar(10)//&
     'RSH R10 R10'//achar(10)//&
     'RSH R7 R7'//achar(10)//&
     'BEV ~+2 R8'//achar(10)//&
     'ADD R7 R7 @MSB'//achar(10)//&
     'RSH R8 R8'//achar(10)//&
     'BNZ .loop2 R10'//achar(10)//&
     'BNZ .loop2 R9'//achar(10)//&
     'HPOP R10'//achar(10)//&
     'HPOP R9'//achar(10)//&
     'HRET'
    character(len=*), parameter :: opengl = &
     '#include <string.h>'//achar(10)//&
     '#include "include/glad.h"'//achar(10)//&
     '#include <GLFW/glfw3.h>'//achar(10)//&
     ''//achar(10)//&
     'static mtx_t windowmtx;'//achar(10)//&
     'GLFWwindow *window;'//achar(10)//&
     ''//achar(10)//&
     '#define WIDTH 512'//achar(10)//&
     '#define HEIGHT 512'//achar(10)//&
     ''//achar(10)//&
     'unsigned int texture;'//achar(10)//&
     ''//achar(10)//&
     'float vertices[] = {'//achar(10)//&
     '    1.0, -1.0, 0.0,'//achar(10)//&
     '    1.0,  1.0, 0.0,'//achar(10)//&
     '   -1.0, -1.0, 0.0,'//achar(10)//&
     '   -1.0,  1.0, 0.0'//achar(10)//&
     '};'//achar(10)//&
     ''//achar(10)//&
     'void framebuffer_size_callback(GLFWwindow* window, int wwidth, int wheight)'//achar(10)//&
     '{'//achar(10)//&
     '    glViewport(0, 0, wwidth, wheight);'//achar(10)//&
     '    mtx_lock(&windowmtx);'//achar(10)//&
     '    data = malloc(wwidth*wheight*4);'//achar(10)//&
     '    width = wwidth;'//achar(10)//&
     '    height = wheight;'//achar(10)//&
     '    mtx_unlock(&windowmtx);'//achar(10)//&
     '    char* title = malloc(64);'//achar(10)//&
     '    sprintf(title,"%dx%d",width,height);'//achar(10)//&
     '    glfwSetWindowTitle(window,title);'//achar(10)//&
     '}'//achar(10)//&
     ''//achar(10)//&
     'char* readFile(char* filename) {'//achar(10)//&
     '  char* buffer = 0;'//achar(10)//&
     '  int len;'//achar(10)//&
     '  FILE *f = fopen(filename, "rb");'//achar(10)//&
     ''//achar(10)//&
     '  if (f) {'//achar(10)//&
     '    fseek(f, 0, SEEK_END);'//achar(10)//&
     '    len = ftell(f);'//achar(10)//&
     '    fseek(f, 0, SEEK_SET);'//achar(10)//&
     '    buffer = malloc(len + 1);'//achar(10)//&
     '    if (buffer) {'//achar(10)//&
     '      fread(buffer, 1, len, f);'//achar(10)//&
     '    }'//achar(10)//&
     '    fclose(f);'//achar(10)//&
     '    buffer[len] = ''\0'';'//achar(10)//&
     '  }'//achar(10)//&
     '  return buffer;'//achar(10)//&
     '}'//achar(10)//&
     ''//achar(10)//&
     'unsigned int makeShader(const char *vertSrc, char *afragSrc, int num) {'//achar(10)//&
     '  char *thing = "\0";'//achar(10)//&
     '  const char *fragSrc = strncat(afragSrc, thing, 16);'//achar(10)//&
     '  unsigned int vertexShader = glCreateShader(GL_VERTEX_SHADER);'//achar(10)//&
     '  glShaderSource(vertexShader, 1, &vertSrc, NULL);'//achar(10)//&
     '  glCompileShader(vertexShader);'//achar(10)//&
     '  // check for shader compile errors'//achar(10)//&
     '  int success;'//achar(10)//&
     '  char infoLog[512];'//achar(10)//&
     '  glGetShaderiv(vertexShader, GL_COMPILE_STATUS, &success);'//achar(10)//&
     '  if (!success) {'//achar(10)//&
     '    glGetShaderInfoLog(vertexShader, 512, NULL, infoLog);'//achar(10)//&
     '    printf("ERROR::SHADER::VERTEX::COMPILATION_FAILED\n%s\n", infoLog);'//achar(10)//&
     '  }'//achar(10)//&
     '  // fragment shader'//achar(10)//&
     '  unsigned int fragmentShader = glCreateShader(GL_FRAGMENT_SHADER);'//achar(10)//&
     '  glShaderSource(fragmentShader, 1, &fragSrc, NULL);'//achar(10)//&
     '  glCompileShader(fragmentShader);'//achar(10)//&
     '  // check for shader compile errors'//achar(10)//&
     '  glGetShaderiv(fragmentShader, GL_COMPILE_STATUS, &success);'//achar(10)//&
     '  if (!success) {'//achar(10)//&
     '    glGetShaderInfoLog(fragmentShader, 512, NULL, infoLog);'//achar(10)//&
     '    printf("ERROR::SHADER::FRAGMENT::COMPILATION_FAILED\n%s\n", infoLog);'//achar(10)//&
     '  }'//achar(10)//&
     '  // link shaders'//achar(10)//&
     '  unsigned int shaderProgram = glCreateProgram();'//achar(10)//&
     '  glAttachShader(shaderProgram, vertexShader);'//achar(10)//&
     '  glAttachShader(shaderProgram, fragmentShader);'//achar(10)//&
     '  glLinkProgram(shaderProgram);'//achar(10)//&
     '  // check for linking errors'//achar(10)//&
     '  glGetProgramiv(shaderProgram, GL_LINK_STATUS, &success);'//achar(10)//&
     '  if (!success) {'//achar(10)//&
     '    glGetProgramInfoLog(shaderProgram, 512, NULL, infoLog);'//achar(10)//&
     '    printf("ERROR::SHADER::PROGRAM::LINKING_FAILED\n%s\n", infoLog);'//achar(10)//&
     '  }'//achar(10)//&
     '  glDeleteShader(vertexShader);'//achar(10)//&
     '  glDeleteShader(fragmentShader);'//achar(10)//&
     '  return shaderProgram;'//achar(10)//&
     '}'//achar(10)//&
     ''//achar(10)//&
     'int main() {'//achar(10)//&
     '    glfwInit();'//achar(10)//&
     '    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);'//achar(10)//&
     '    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 1);'//achar(10)//&
     '    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);'//achar(10)//&
     ''//achar(10)//&
     '#ifdef __APPLE__'//achar(10)//&
     '  glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);'//achar(10)//&
     '#endif'//achar(10)//&
     '    window = glfwCreateWindow(WIDTH, HEIGHT, "", NULL, NULL);'//achar(10)//&
     '    if (!window) {'//achar(10)//&
     '        glfwTerminate();'//achar(10)//&
     '        return -1;'//achar(10)//&
     '    }'//achar(10)//&
     ''//achar(10)//&
     '    glfwMakeContextCurrent(window);'//achar(10)//&
     ''//achar(10)//&
     '    if (!gladLoadGLLoader((GLADloadproc)glfwGetProcAddress)) {'//achar(10)//&
     '        printf("glad init failed");'//achar(10)//&
     '        return -1;'//achar(10)//&
     '    }'//achar(10)//&
     ''//achar(10)//&
     '    glfwGetFramebufferSize(window, &width, &height);'//achar(10)//&
     '    glViewport(0, 0, width, height);'//achar(10)//&
     '    data = malloc(width*height*4);'//achar(10)//&
     '    char* title = malloc(64);'//achar(10)//&
     '    sprintf(title,"%dx%d",width,height);'//achar(10)//&
     '    glfwSetWindowTitle(window,title);'//achar(10)//&
     '    glfwSetFramebufferSizeCallback(window, framebuffer_size_callback);'//achar(10)//&
     ''//achar(10)//&
     '    unsigned int VBO, VAO;'//achar(10)//&
     ''//achar(10)//&
     '    glGenVertexArrays(1, &VAO);'//achar(10)//&
     '    glGenBuffers(1, &VBO);'//achar(10)//&
     '    glBindVertexArray(VAO);'//achar(10)//&
     ''//achar(10)//&
     '    glBindBuffer(GL_ARRAY_BUFFER, VBO);'//achar(10)//&
     '    glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);'//achar(10)//&
     ''//achar(10)//&
     '    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(float), (void *)0);'//achar(10)//&
     '    glEnableVertexAttribArray(0);'//achar(10)//&
     ''//achar(10)//&
     ''//achar(10)//&
     '    const char *vertexShaderSource = "#version 330 core\n"'//achar(10)//&
     '                                 "layout (location = 0) in vec3 aPos;\n"'//achar(10)//&
     '                                 "void main()\n"'//achar(10)//&
     '                                 "{\n"'//achar(10)//&
     '                                 "   gl_Position = vec4(aPos, 1.0);\n"'//achar(10)//&
     '                                 "}\0";'//achar(10)//&
     '    unsigned int shaderProgram ='//achar(10)//&
     '      makeShader(vertexShaderSource, readFile("include/shader.fs"), 0);'//achar(10)//&
     ''//achar(10)//&
     '    unsigned int framebuffer;'//achar(10)//&
     '    glGenFramebuffers(1, &framebuffer);'//achar(10)//&
     '    glBindFramebuffer(GL_FRAMEBUFFER, framebuffer);'//achar(10)//&
     '    unsigned int textureColorbuffer;'//achar(10)//&
     '    glGenTextures(1, &textureColorbuffer);'//achar(10)//&
     '    glBindTexture(GL_TEXTURE_2D, textureColorbuffer);'//achar(10)//&
     '    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, WIDTH, HEIGHT, 0, GL_RGB,'//achar(10)//&
     '               GL_UNSIGNED_BYTE, NULL);'//achar(10)//&
     '    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);'//achar(10)//&
     '    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);'//achar(10)//&
     '    glBindTexture(GL_TEXTURE_2D, 0);'//achar(10)//&
     '    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D,'//achar(10)//&
     '                         textureColorbuffer, 0);'//achar(10)//&
     ''//achar(10)//&
     '    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {'//achar(10)//&
     '        printf("main framebuffer is not complete");'//achar(10)//&
     '    }'//achar(10)//&
     '    glBindFramebuffer(GL_FRAMEBUFFER, 0);'//achar(10)//&
     ''//achar(10)//&
     '    glGenTextures(1, &texture);'//achar(10)//&
     '    glActiveTexture(GL_TEXTURE0);'//achar(10)//&
     '    glBindTexture(GL_TEXTURE_2D, texture);'//achar(10)//&
     '    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_NEAREST);'//achar(10)//&
     '    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);'//achar(10)//&
     '    //glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, data);'//achar(10)//&
     ''//achar(10)//&
     ''//achar(10)//&
     '// setup threading'//achar(10)//&
     '    status = 0;'//achar(10)//&
     '    mtx_init(&windowmtx,mtx_plain);'//achar(10)//&
     '    thrd_t renderthread;'//achar(10)//&
     '    mtxptr = &windowmtx;'//achar(10)//&
     '    windowptr = window;'//achar(10)//&
     '    thrd_create(&renderthread,run,NULL);'//achar(10)//&
     '//'//achar(10)//&
     '    glBindFramebuffer(GL_FRAMEBUFFER, 0);'//achar(10)//&
     '    glfwSwapInterval(0);'//achar(10)//&
     '    while(!glfwWindowShouldClose(window)) {'//achar(10)//&
     '        glUseProgram(shaderProgram);'//achar(10)//&
     '        glBindTexture(GL_TEXTURE_2D, texture);'//achar(10)//&
     '        glUniform1i(glGetUniformLocation(shaderProgram, "sizex"), width);'//achar(10)//&
     '        glUniform1i(glGetUniformLocation(shaderProgram, "sizey"), height);'//achar(10)//&
     '        glUniform1i(glGetUniformLocation(shaderProgram, "pixels"), 0);'//achar(10)//&
     ''//achar(10)//&
     '        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, data);'//achar(10)//&
     '        glGenerateMipmap(GL_TEXTURE_2D);'//achar(10)//&
     '        glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);'//achar(10)//&
     ''//achar(10)//&
     '        glfwSwapBuffers(window);'//achar(10)//&
     '        glfwPollEvents();'//achar(10)//&
     '    }'//achar(10)//&
     '    mtx_lock(mtxptr);'//achar(10)//&
     '    status = 1;'//achar(10)//&
     '    mtx_unlock(mtxptr);'//achar(10)//&
     '    glfwTerminate();'//achar(10)//&
     '    thrd_join(renderthread, NULL);'//achar(10)//&
     '}'
end module
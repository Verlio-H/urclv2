module emit
    use inst
    implicit none
    integer :: maxregplus1
    integer :: maxreg

    character(len=:), allocatable :: colormode
   contains

    subroutine init()
        if (arch(:1)=='C') then
            write(2,'(A)') '#include <stdio.h>',&
            &'#include <stdlib.h>',&
            &'#include <math.h>',&
            &'#include <string.h>',&
            &'#ifdef _WIN32',&
            &'#include <conio.h>',&
            &'#define HEADER " "',&
            &'#else',&
            &'#include <termios.h>',&
            &'struct termios orig_termios;',&
            &'void resetMode() {',&
            &'tcsetattr(0,TCSAFLUSH,&orig_termios);',&
            &'}',&
            &'void enableMode() {',&
            &'tcgetattr(0,&orig_termios);',&
            &'atexit(resetMode);',&
            &'struct termios raw = orig_termios;',&
            &'raw.c_lflag &= ~(ECHO | ICANON);',&
            &'tcsetattr(0, TCSAFLUSH, &raw);',&
            &'}',&
            &'#define HEADER enableMode()',&
            &'#define getch getchar',&
            &'#endif',&
            &'#define const_SIZE8 sizeof(unsigned char)',&
            &'#define const_SIZE16 sizeof(unsigned short)',&
            &'#define const_SIZE32 sizeof(unsigned int)',&
            &'#define const_SIZEADDR sizeof(void*)',&
            &'#define const_SIZEREAL sizeof(float)',&
            &'#define const_SIZELREAL sizeof(double)',&
            &'union tmp {',&
            &'unsigned char v8;',&
            &'unsigned short v16;',&
            &'unsigned int v32;',&
            &'unsigned long long v64;',&
            &'void* vADDR;',&
            &'float vREAL;',&
            &'double vLREAL;',&
            &'};',&
            &'unsigned char* data = NULL;',&
            &'unsigned char* data2;',&
            &'unsigned char* write;',&
            &'int buffered = 0;',&
            &'int width, height, status;',&
            &'int width2, height2;',&
            &'void* mtxptr;',&
            &'void* windowptr;',&
            &'void* sp;'
            if (incopengl.or.incthread) then
                write(2,'(A)') '#include "include/c11threads.h"'
                write(2,'(A)') 'thrd_t* threads;'
                write(2,'(A)') 'int sizethreads;'
            end if
            if (incopengl) then
                
                write(2,'(A)') '#include "include/glad.h"','#include <GLFW/glfw3.h>'
            end if
        else if (arch=='IRIS') then
            write(2,'(A)') 'BITS 16','MINREG 25'
        else if (arch=='SILK') then
        else
            print '(A)', 'init not implemented for this architecture'
        end if
    end subroutine

    subroutine end()
        if (arch(:1)=='C') then
            write(2,'(A)') 'return 0;', '}'
            if (incopengl) then
                write(2,'(A)') opengl
            else
                write(2,'(A)') 'int main() { '
                if (incopengl.or.incthread) then
                    write(2,'(A)') 'threads = malloc(0*sizeof(thrd_t));','sizethreads = 0;'
                end if
                write(2,'(A)') 'run();'
                if (incopengl.or.incthread) then
                    write(2,'(A)') 'for (int i=0; i<sizethreads; ++i) thrd_join(*(threads+i),NULL);'
                end if
                write(2,'(A)') '}'
            end if
        else if (arch=='IRIS') then
            write(2,'(A)') 'HLT'
            if (incprint32) then
                write(2,'(A)') print32
            end if
            if (incdiv32) then
                write(2,'(A)') div32
            end if
        else if (arch=='SILK') then
            write(2,'(A)') 'SCAL FREE R1'
            write(2,'(A)') 'SCAL EXIT R0'
        else
            print '(A)', 'end not implemented for this architecture'
        end if
    end subroutine

    subroutine label(arg1)
        character(len=:), allocatable, intent(in) :: arg1
        if (arch(:1)=='C') then
            call app('urcl'//itoa(id)//'_'//arg1(2:)//': ;')
        else if (arch=='IRIS') then
            call app('.label'//itoa(id)//'_'//arg1(2:))
        end if
    end subroutine

    subroutine minmem(arg1)
        character(len=:), allocatable, intent(in) :: arg1
        integer :: temp
        if (memdec) then
            call throw('attempt to redefine size of memory')
            
        end if
        memdec = .true.
        if (arch(:1)=='C') then
            call app(c_type(memsze)//' mem['//arg1//'];')
        else if (arch=='IRIS') then
            if (memsze==32) then
                read (arg1, *) temp
                call app('MINHEAP '//itoa(temp*2))
            else
                call app('MINHEAP '//arg1)
            end if
        end if
    end subroutine

    subroutine minstack(arg1)
        character(len=:), allocatable, intent(in) :: arg1
        if (stackdec) then
            call throw('attempt to redefine size of stack')
            
        end if
        stackdec = .true.
        if (arch(:1)=='C') then
            call app(c_type(memsze)//' stack['//arg1//'];')
            call app('sp = stack;')
        end if
    end subroutine

    subroutine mincstack(arg1)
        character(len=:), allocatable, intent(in) :: arg1
        if (cstackdec) then
            call throw('attempt to redefine size of call stack')
            
        end if
        cstackdec = .true.
        if (arch(:1)=='C') then
            call app('void* cstack['//arg1//'];')
            call app('int csp = 0;')
        end if
    end subroutine

    subroutine define(name, value, vars, dws)
        type(DW), allocatable, intent(in) :: dws(:)
        character(len=:), allocatable, intent(in) :: name
        character(len=:), allocatable, intent(in) :: value
        type(variable), allocatable, intent(in) :: vars(:)

        character(len=:), allocatable :: result
        type(defined) :: tdefine
        integer :: type
        result = parseArg(value, type, vars, dws)
        tdefine%name = name
        tdefine%value = result(2:)
        tdefine%int = .false.
        if (result(:1)=='I'.and.type>=8) then
            read(result(2:), *) tdefine%ivalue
            tdefine%int = .true.
        end if
        defines = [defines, tdefine]
    end subroutine

    integer function typesize(type)
        integer, intent(in) :: type
        if (arch(:1)=='C') then
            select case (type)
            case (1)
                typesize = 64
            case (2)
                typesize = 32
            case (3)
                typesize = 64
            case default
                typesize = type
            end select
        else
            call throw('internal error: typesize not implemented for this architecture')
            stop !never executed but here to stop gfortran from complaining
        end if
    end function

    subroutine irisimm(addr,result2,type1,type2)
        integer, intent(in) :: addr, type1, type2
        character(len=:), allocatable, intent(in) :: result2
        integer :: int
        if (type1/=32) then
            if (type2==32) then
                call throw('arg1 of builtin instruction must be of size 32 if arg2 is size 32 for arch IRIS')
                
            end if
            if (type1==8.and.type2/=8) then
                call throw('warning: destination of imm is smaller than immediate size')
            end if
            if (addr<=maxreg) then
                if (type1==8.and.type2/=8) then
                    call app('IMM R'//itoa(addr)//' '//result2//' 0xFF')
                else
                    call app('IMM R'//itoa(addr)//' '//result2)
                end if
            else
                if (type1==8.and.type2/=8) then
                    if (arch=='IRIS') then
                        call app('IMM R21 '//result2//' 0xFF')
                        call app('STR M'//itoa(addr-maxregplus1)//' R21')
                    else if (arch=='SILK') then
                        call app('IMM R10 '//result2//' 0xFF')
                        call app('IMM R15 '//itoa(addr-maxregplus1))
                        call app('LSTR R1 R10 R15')
                    end if
                else
                    call app('STR M'//itoa(addr-maxregplus1)//' '//result2)
                end if
            end if
        else
            read(result2,*) int
            if (addr<=maxregplus1) then
                call app('IMM R'//itoa(addr)//' '//itoa(iand(int,2**16-1)))
            else
                call app('STR M'//itoa(addr-maxregplus1-1)//' '//itoa(iand(int,2**16-1)))
            end if
            if (addr<=maxreg) then
                call app('IMM R'//itoa(addr+1)//' '//itoa(shiftr(int,16)))
            else
                call app('STR M'//itoa(addr-maxregplus1)//' '//itoa(shiftr(int,16)))
            end if
        end if
    end subroutine

    subroutine imm(arg1,arg2,vars, dws)
        type(DW), allocatable, intent(in) :: dws(:)
        type(variable), allocatable, intent(in) :: vars(:)
        character(len=:), allocatable, intent(in) :: arg1, arg2
        character(len=:), allocatable :: result1, result2
        integer type1, type2, addr
        result1 = parseArg(arg1, type1, vars, dws)
        result2 = parseArg(arg2, type2, vars, dws) !only 2nd arg type is important
        if (result1(:1)/='V') then
            call throw('arg1 of IMM must be a variable')
            
        else if (result2(:1)/='I') then
            call throw('arg2 of IMM must be an immediate value')
            
        end if
        result1 = result1(2:)
        result2 = result2(2:)
        if (arch(:1)=='C') then
            if (type2==8.or.type2==16) type2 = 32
            call app('tmp1.v'//trim(typestr(type2))//'='//result2//';')
            result2 = 'tmp1.v'//trim(typestr(type1))
            call vars(getvar_index(vars, result1))%set(result2)
        else if (arch=='IRIS') then
            addr = vars(getvar_index(vars, result1))%location
            call irisimm(addr,result2,type1,type2)
        end if
    end subroutine

    subroutine standard3Op(op, arg1, arg2, arg3, vars, dws)
        type(DW), allocatable, intent(in) :: dws(:)
        type(variable), allocatable, intent(in) :: vars(:)
        character(len=:), allocatable, intent(in) :: op, arg1, arg2, arg3
        character(len=:), allocatable :: result1, result2, result3, stype2, stype3
        character(len=:), allocatable :: output1, output2, output3
        character(len=:), allocatable :: output1u, output2u, output3u
        integer type1, type2, type3, itemp, itemp2, itemp3
        result1 = parseArg(arg1, type1, vars, dws)
        result2 = parseArg(arg2, type2, vars, dws)
        result3 = parseArg(arg3, type3, vars, dws)
        select case (op)
        case ('LLOD')
            if (type2/=1) then
                call throw('arg2 of LLOD must be of type ADDR')
            end if
        case ('BGE','BRG','BLE','BRL','BRE','BNE','BRC','BNC','SBGE','SBRG','SBLE','SBRL','SBRC','SBNC','FBGE',&
             &'FBRG','FBLE','FBRL','FBRE','FBNE','LFBGE','LFBRG','LFBLE','LFBRL','LFBRE','LFBNE')
            if (type1/=1) then
                call throw('arg1 of '//op//' must be of type ADDR')
            end if
        case default
            if (result1(:1)/='V') then
                call throw('arg1 of '//op//' must be a variable')
            end if
        end select
        if (arch=='IRIS') then
            select case (op)
            case ('BGE','BLE','BRG','BRL','BRE','BNE','BRC','BNC','SBRG','SBRL','SBGE','SBLE')
                if (type2/=32.and.type3/=32) then
                    call parseSmall(output1,result1,1,vars)
                    call parseSmall(output2,result2,2,vars)
                    call parseSmall(output3,result3,3,vars)
                    call app(op//' '//output1//' '//output2//' '//output3)
                else
                    call parseSmall(output1,result1,1,vars)
                    call parseBig(output2,output2u,result2,2,vars,type2)
                    call parseBig(output3,output3u,result3,3,vars,type3)
                    select case (op)
                    case ('BGE')
                        if (output2u/=''.and.output3u/='') then
                            call app('BRG '//output1//' '//output2u//' '//output3u)
                            call app('BNE ~+2 '//output2u//' '//output3u)
                        else if (output3u=='') then
                            call app('BNZ '//output1//' '//output2u)
                        else if (output2u=='') then
                            call app('BNZ ~+2 '//output3u)
                        end if
                        call app('BGE '//output1//' '//output2//' '//output3)
                    case ('BRG')
                        if (output2u/=''.and.output3u/='') then
                            call app('BRG '//output1//' '//output2u//' '//output3u)
                            call app('BNE ~+2 '//output2u//' '//output3u)
                        else if (output3u=='') then
                            call app('BNZ '//output1//' '//output2u)
                        else if (output2u=='') then
                            call app('BNZ ~+2 '//output3u)
                        end if
                        call app('BRG '//output1//' '//output2//' '//output3)
                    case ('BLE')
                        if (output2u/=''.and.output3u/='') then
                            call app('BRL '//output1//' '//output2u//' '//output3u)
                            call app('BNE ~+2 '//output2u//' '//output3u)
                        else if (output3u=='') then
                            call app('BNZ ~+2 '//output2u)
                        else if (output2u=='') then
                            call app('BNZ '//output1//' '//output3u)
                        end if
                        call app('BLE '//output1//' '//output2//' '//output3)
                    case ('BRL')
                        if (output2u/=''.and.output3u/='') then
                            call app('BRL '//output1//' '//output2u//' '//output3u)
                            call app('BNE ~+2 '//output2u//' '//output3u)
                        else if (output3u=='') then
                            call app('BNZ ~+2 '//output2u)
                        else if (output2u=='') then
                            call app('BNZ '//output1//' '//output3u)
                        end if
                        call app('BRL '//output1//' '//output2//' '//output3)
                    case ('BRE')
                        if (output2u/=''.and.output3u/='') then
                            call app('BNE ~+2 '//output2u//' '//output3u)
                        else if (output3u=='') then
                            call app('BNE ~+2 '//output2u//' R0')
                        else if (output2u=='') then
                            call app('BNE ~+2 '//output3u//' R0')
                        end if
                        call app('BRE '//output1//' '//output2//' '//output3)
                    case ('BNE')
                        if (output2u/=''.and.output3u/='') then
                            call app('BNE '//output1//' '//output2u//' '//output3u)
                        else if (output3u=='') then
                            call app('BNE '//output1//' '//output2u//' R0')
                        else if (output2u=='') then
                            call app('BNE '//output1//' '//output3u//' R0')
                        end if
                        call app('BNE '//output1//' '//output2//' '//output3)
                    case ('BRC')
                        if (output2u/=''.and.output3u/='') then
                            call app('BRC '//output1//' '//output2u//' '//output3u)
                            call app('ADD R25 '//output2u//' '//output3u)
                            call app('BNE ~+2 R25 @MAX')
                        else if (output3u=='') then
                            call app('BNE ~+2 @MAX '//output3u)
                        else if (output2u=='') then
                            call app('BNE ~+2 @MAX '//output2u)
                        end if
                        call app('BRC '//output1//' '//output2//' '//output3)
                    case ('BNC')
                        if (output2u/=''.and.output3u/='') then
                            call app('BRC ~+4 '//output2u//' '//output3u)
                            call app('ADD R25 '//output2u//' '//output3u)
                            call app('BNE '//output1//' R25 @MAX')
                        else if (output3u=='') then
                            call app('BNE '//output1//' '//output3u//' @MAX')
                        else if (output2u=='') then
                            call app('BNE '//output1//' '//output2u//' @MAX')
                        end if
                        call app('BNC '//output1//' '//output2//' '//output3)
                    case ('SBRG')
                        if (output2u/=''.and.output3u/='') then
                            call app('SBRG '//output1//' '//output2u//' '//output3u)
                            call app('BNE ~+6 '//output2u//' '//output3u)
                        else if (output3u=='') then
                            call app('SBRG '//output1//' '//output2u//' 0')
                        else if (output2u=='') then
                            call app('SBRL ~+6 '//output3u//' -1')
                        end if
                        call app('BRN ~+3 '//output2u)
                        call app('BRN '//output1//' '//output3u)
                        call app('JMP ~+2')
                        call app('BRP ~+2 '//output3u)
                        call app('BRG '//output1//' '//output2//' '//output3)
                    case ('SBRL')
                        if (output2u/=''.and.output3u/='') then
                            call app('SBRL '//output1//' '//output2u//' '//output3u)
                            call app('BNE ~+6 '//output2u//' '//output3u)
                        else if (output3u=='') then
                            call app('SBRG ~+6 '//output2u//' 0')
                        else if (output2u=='') then
                            call app('SBRL '//output1//' '//output3u//' -1')
                        end if
                        call app('BRN ~+3 '//output2u)
                        call app('BRN ~+4 '//output3u)
                        call app('JMP ~+2')
                        call app('BRP '//output1//' '//output3u)
                        call app('BRG '//output1//' '//output2//' '//output3)
                    case ('SBGE')
                        if (output2u/=''.and.output3u/='') then
                            call app('SBRG '//output1//' '//output2u//' '//output3u)
                            call app('BNE ~+6 '//output2u//' '//output3u)
                        else if (output3u=='') then
                            call app('SBRG '//output1//' '//output2u//' 0')
                        else if (output2u=='') then
                            call app('SBRL ~+6 '//output3u//' -1')
                        end if
                        call app('BRN ~+3 '//output2u)
                        call app('BRN '//output1//' '//output3u)
                        call app('JMP ~+2')
                        call app('BRP ~+2 '//output3u)
                        call app('BGE '//output1//' '//output2//' '//output3)
                    case default
                        call throw('32 bit translation not implemented for '//op,.false.)
                    end select
                end if
            case ('LLOD')
                result1 = result1(2:)
                itemp = iachar(result3(:1))
                itemp2 = iachar(result3(2:2))
                call parseSmall(output2,result2,2,vars)
                call parseSmall(output3,result3,3,vars)
                call vars(getvar_index(vars,result1))%set('LLOD '//output2//' '//output3)
                if (type1==32) then
                    if (achar(itemp)=='V'.or.achar(itemp2)=='.'.or.achar(itemp2)=='M') then
                        call app('INC R25 '//output3)
                        call vars(getvar_index(vars,result1))%set('LLOD '//output2//' R25',.true.)
                    else
                        read(output3,*) itemp
                        call vars(getvar_index(vars,result1))%set('LLOD '//output2//' '//itoa(itemp+1),.true.)
                    end if
                end if
            case ('ADD','SUB','MLT','DIV','MOD','BSL','BSR','SBSR','OR','NOR','AND','NAND','XOR','XNOR')
                if (type1/=32) then
                    call parseSmall(output2,result2,2,vars)
                    call parseSmall(output3,result3,3,vars)
                    result1 = result1(2:)
                    select case (op)
                    case ('SBSR')
                        call vars(getvar_index(vars,result1))%set('BSS '//output2//' '//output3)
                    case default
                        call vars(getvar_index(vars,result1))%set(op//' '//output2//' '//output3)
                    end select
                    if (type1==8) then
                        call parseSmall(output1,result1,1,vars)
                        call vars(getvar_index(vars,result1))%set('AND '//output1//' 0xFF')
                    end if
                else
                    call parseBig(output2,output2u,result2,2,vars,type2)
                    call parseBig(output3,output3u,result3,3,vars,type3)
                    result1 = result1(2:)

                    select case (op)
                    case ('ADD')
                        if (output2u/=''.and.output3u/='') then
                            call app('ADD R25 '//output2u//' '//output3u)
                        else if (output2u==''.and.output3u=='') then
                            call app('MOV R25 R0')
                        else if (output2u=='') then
                            call app('MOV R25 '//output3u)
                        else if (output3u=='') then
                            call app('MOV R25 '//output2u)
                        end if
                        call app('BNC ~+2 '//output2//' '//output3)
                        call app('INC R25 R25')
                        call vars(getvar_index(vars,result1))%set(' R25',.true.)
                        call vars(getvar_index(vars,result1))%set('ADD '//output2//' '//output3)
                    case ('SUB')
                        call app('NOT R23 '//output3)
                        call app('NOT R24 '//output3u)
                        call app('BNC ~+2 R23 1')
                        call app('INC R24 R24')
                        call app('INC R23 R23')
                        call app('ADD R25 '//output2u//' R24')
                        call app('BNC ~+2 '//output2//' R23')
                        call app('INC R25 R25')
                        call vars(getvar_index(vars,result1))%set('ADD '//output2//' R23')
                        call vars(getvar_index(vars,result1))%set(' R25',.true.)
                    case ('MLT')
                        itemp = vars(getvar_index(vars,result1))%location
                        if (output2(:1)=='R') then
                            read(output2(2:),*) itemp2
                        else
                            itemp2 = 0
                        end if
                        if (output3(:1)=='R') then
                            read(output2(2:),*) itemp3
                        else
                            itemp3 = 0
                        end if
                        if (itemp==itemp2.or.itemp==itemp3.or.itemp>=maxreg) then
                            do itemp=1,24
                                if (itemp/=itemp2.and.itemp/=itemp2+1.and.itemp+1/=itemp2) then
                                    if (itemp/=itemp3.and.itemp/=itemp3+1.and.itemp+1/=itemp3) then
                                        exit
                                    end if
                                end if
                            end do
                        end if
                        if (itemp/=vars(getvar_index(vars,result1))%location) then
                            call app('HPSH R'//itoa(itemp))
                            call app('HPSH R'//itoa(itemp+1))
                        end if
                        call app('UMLT R'//itoa(itemp+1)//' '//output2//' '//output3)
                        if (output3u/=''.or.output2u/='') then
                            if (output3u/=''.and.output2u/='') then
                                call app('MLT R'//itoa(itemp)//' '//output2//' '//output3u)
                                call app('ADD R'//itoa(itemp+1)//' R'//itoa(itemp+1)//' R'//itoa(itemp))
                                call app('MLT R'//itoa(itemp)//' '//output2u//' '//output3)
                                call app('ADD R'//itoa(itemp+1)//' R'//itoa(itemp+1)//' R'//itoa(itemp))
                            else if (output3u=='') then
                                call app('MLT R'//itoa(itemp)//' '//output2u//' '//output3)
                                call app('ADD R'//itoa(itemp+1)//' R'//itoa(itemp+1)//' R'//itoa(itemp))
                            else if (output2u=='') then
                                call app('MLT R'//itoa(itemp)//' '//output2//' '//output3u)
                                call app('ADD R'//itoa(itemp+1)//' R'//itoa(itemp+1)//' R'//itoa(itemp))
                            end if
                        end if
                        call app('MLT R'//itoa(itemp)//' '//output2//' '//output3)
                        if (itemp/=vars(getvar_index(vars,result1))%location) then
                            output2 = 'R'//repeat(' ',11)
                            write(output2(2:),'(I0)') itemp
                            call vars(getvar_index(vars,result1))%set(' '//trim(output2))
                            write(output2(2:),'(I0)') itemp+1
                            call vars(getvar_index(vars,result1))%set(' '//trim(output2),.true.)
                            call app('HPOP R'//itoa(itemp+1))
                            call app('HPOP R'//itoa(itemp))
                        end if
                    case ('DIV','MOD')
                        incdiv32 = .true.
                        itemp = vars(getvar_index(vars,result1))%location
                        if (itemp/=2.and.itemp/=3) then
                            call app('HPSH R3')
                        end if
                        if (itemp/=3.and.itemp/=4) then
                            call app('HPSH R4')
                        end if
                        if (itemp/=4.and.itemp/=5) then
                            call app('HPSH R5')
                        end if
                        if (itemp/=5.and.itemp/=6) then
                            call app('HPSH R6')
                        end if
                        call app('HPSH R7')
                        call app('HPSH R8')
                        call app('MOV R5 '//output2)
                        if (output2u=='') then
                            call app('MOV R6 R0')
                        else
                            call app('MOV R6 '//output2u)
                        end if
                        call app('MOV R7 '//output3)
                        if (output3u=='') then
                            call app('MOV R8 R0')
                        else
                            call app('MOV R8 '//output3u)
                        end if
                        call app('HCAL .div32')
                        call app('HPOP R8')
                        call app('HPOP R7')
                        if (op=='DIV'.and.itemp/=3) then
                            if (itemp==4) then
                                call vars(getvar_index(vars,result1))%set(' R4',.true.)
                                call vars(getvar_index(vars,result1))%set(' R3')
                            else
                                call vars(getvar_index(vars,result1))%set(' R3')
                                call vars(getvar_index(vars,result1))%set(' R4',.true.)
                            end if
                        else if (itemp/=5) then
                            if (itemp==6) then
                                call vars(getvar_index(vars,result1))%set(' R6',.true.)
                                call vars(getvar_index(vars,result1))%set(' R5')
                            else
                                call vars(getvar_index(vars,result1))%set(' R5')
                                call vars(getvar_index(vars,result1))%set(' R6',.true.)
                            end if
                        end if
                        if (itemp/=5.and.itemp/=6) then
                            call app('HPOP R6')
                        end if
                        if (itemp/=4.and.itemp/=5) then
                            call app('HPOP R5')
                        end if
                        if (itemp/=3.and.itemp/=4) then
                            call app('HPOP R4')
                        end if
                        if (itemp/=2.and.itemp/=3) then
                            call app('HPOP R3')
                        end if
                    case ('BSL')
                        if (output3(:1)/='R') then
                            read(output3,*) itemp
                            if (itemp<16) then
                                call app('AND R25 '//output2//' '//itoa(2**16-2**(16-itemp)))
                                call app('BSL R20 '//output2u//' '//output3)
                                call app('BSR R25 R25 '//itoa(16-itemp))
                                call vars(getvar_index(vars,result1))%set('ADD R20 R25',.true.)
                                call vars(getvar_index(vars,result1))%set('BSL '//output2//' '//output3)
                            else if (itemp==16) then
                                call vars(getvar_index(vars,result1))%set(' '//output2,.true.)
                                call vars(getvar_index(vars,result1))%set(' R0')
                            else
                                write(output3,'(I0)') itemp-16
                                call vars(getvar_index(vars,result1))%set('BSL '//output2//' '//trim(output3),.true.)
                                call vars(getvar_index(vars,result1))%set(' R0')
                            end if
                        else
                            call app('SUB R19 16 '//output3)
                            call app('BRN .unique'//itoa(unique)//' R19')
                            call app('BSS R25 @MSB '//output3)
                            call app('BSL R20 '//output2u//' '//output3)
                            call app('AND R25 R25 '//output2)
                            call app('BSR R25 R25 R19')
                            call vars(getvar_index(vars,result1))%set('ADD R20 R25',.true.)
                            call vars(getvar_index(vars,result1))%set('BSL '//output2//' '//output3)
                            call app('JMP .unique'//itoa(unique+1))
                            call app('.unique'//itoa(unique))
                            unique = unique+1
                            call vars(getvar_index(vars,result1))%set('BSL '//output2//' R19',.true.)
                            call vars(getvar_index(vars,result1))%set(' R0')
                            call app('.unique'//itoa(unique))
                            unique = unique+1
                        end if
                    case ('BSR','SBSR')
                        if (output3(:1)/='R') then
                            read(output3,*) itemp
                            if (itemp<16) then
                                call app('AND R25 '//output2u//' '//itoa(2**itemp-1))
                                call app('BSR R20 '//output2//' '//output3)
                                call app('BSL R25 R25 '//itoa(16-itemp))
                                call vars(getvar_index(vars,result1))%set('ADD R20 R25')
                                if (op=='BSR') then
                                    call vars(getvar_index(vars,result1))%set('BSR '//output2u//' '//output3,.true.)
                                else
                                    call vars(getvar_index(vars,result1))%set('BSS '//output2u//' '//output3,.true.)
                                end if
                            else if (itemp==16) then
                                call vars(getvar_index(vars,result1))%set(' '//output2u)
                                if (op=='BSR') then
                                    call vars(getvar_index(vars,result1))%set(' R0',.true.)
                                else
                                    call parseBig(output1,output1u,result1,1,vars,type1)
                                    call vars(getvar_index(vars,result1))%set('SSETL '//output1u//' R0',.true.)
                                end if
                            else
                                write(output3,'(I0)') itemp-16
                                if (op=='BSR') then
                                    call vars(getvar_index(vars,result1))%set('BSR '//output2u//' '//trim(output3))
                                    call vars(getvar_index(vars,result1))%set(' R0',.true.)
                                else
                                    call parseBig(output1,output1u,result1,1,vars,type1)
                                    call vars(getvar_index(vars,result1))%set('BSS '//output2u//' '//trim(output3))
                                    call vars(getvar_index(vars,result1))%set('SSETL '//output1u//' R0',.true.)
                                end if
                            end if
                        else
                        end if
                    case ('OR')
                        call vars(getvar_index(vars,result1))%set('OR '//output2//' '//output3)
                        if (output2u/=''.and.output3u/='') then
                            call vars(getvar_index(vars,result1))%set('OR '//output2u//' '//output3u,.true.)
                        else if (output2u/='') then
                            call vars(getvar_index(vars,result1))%set(' '//output2u,.true.)
                        else
                            call vars(getvar_index(vars,result1))%set(' '//output3u,.true.)
                        end if
                    case ('NOR')
                        call vars(getvar_index(vars,result1))%set('NOR '//output2//' '//output3)
                        if (output2u/=''.and.output3u/='') then
                            call vars(getvar_index(vars,result1))%set('NOR '//output2u//' '//output3u,.true.)
                        else if (output2u/='') then
                            call vars(getvar_index(vars,result1))%set('NOT '//output2u,.true.)
                        else
                            call vars(getvar_index(vars,result1))%set('NOT '//output3u,.true.)
                        end if
                    case ('AND')
                        call vars(getvar_index(vars,result1))%set('AND '//output2//' '//output3)
                        if (output2u/=''.and.output3u/='') then
                            call vars(getvar_index(vars,result1))%set('AND '//output2u//' '//output3u,.true.)
                        else
                            call vars(getvar_index(vars,result1))%set(' R0',.true.)
                        end if
                    case ('NAND')
                        call vars(getvar_index(vars,result1))%set('NAND '//output2//' '//output3)
                        if (output2u/=''.and.output3u/='') then
                            call vars(getvar_index(vars,result1))%set('NAND '//output2u//' '//output3u,.true.)
                        else
                            call vars(getvar_index(vars,result1))%set(' -1',.true.)
                        end if
                    case ('XOR')
                        call vars(getvar_index(vars,result1))%set('XOR '//output2//' '//output3)
                        if (output2u/=''.and.output3u/='') then
                            call vars(getvar_index(vars,result1))%set('XOR '//output2u//' '//output3u,.true.)
                        else if (output2u=='') then
                            call vars(getvar_index(vars,result1))%set(' '//output3u,.true.)
                        else
                            call vars(getvar_index(vars,result1))%set(' '//output2u,.true.)
                        end if
                    case ('XNOR')
                        call vars(getvar_index(vars,result1))%set('XOR '//output2//' '//output3)
                        if (output2u/=''.and.output3u/='') then
                            call vars(getvar_index(vars,result1))%set('XOR '//output2u//' '//output3u,.true.)
                        else if (output2u=='') then
                            call vars(getvar_index(vars,result1))%set(' '//output3u,.true.)
                        else
                            call vars(getvar_index(vars,result1))%set(' '//output2u,.true.)
                        end if
                    case default
                        call throw('32 bit translation not implemented for '//op,.false.)
                    end select
                end if
            case ('FADD','LFADD','FSUB','LFSUB','FMLT','LFMLT','FDIV','LFDIV','FMOD')
                call parseSmall(output2,result2,2,vars)
                call parseSmall(output3,result3,3,vars)
                result1 = result1(2:)
                block
                    character(len=:), allocatable :: optmp
                    optmp = op
                    if (optmp(:1)=='L') optmp = optmp(2:)
                    select case (optmp)
                    case ('FMOD')
                        call app('FDIV R25 '//output2//' '//output3)
                        call app('FTOI R25 R25')
                        call app('ITOF R25 R25')
                        call app('FMLT R25 R25 '//output3)
                        call vars(getvar_index(vars,result1))%set('FSUB '//output2//' R25')
                    case default
                        call vars(getvar_index(vars,result1))%set(optmp//' '//output2//' '//output3)
                    end select
                end block
            case ('FBRL','LFBRL','FBRG','LFBRG','FBLE','LFBLE','FBGE','LFBGE','FBRE','LFBRE','FBNE','LFBNE')
                call parseSmall(output2,result2,2,vars)
                call parseSmall(output3,result3,3,vars)
                call parseSmall(output1,result1,1,vars)
                if (op(:1)=='L') then
                    call app('S'//op(3:)//' '//output1//' '//output2//' '//output3)
                else
                    call app('S'//op(2:)//' '//output1//' '//output2//' '//output3)
                end if
            case default
                call throw('unknown instruction '//op//' for arch IRIS')
                
            end select
        else if (arch(:1)=='C') then
            result1 = result1(2:)
            result2 = result2(2:)
            result3 = result3(2:)
            stype2 = signed_c_type(type2)
            stype3 = signed_c_type(type3)

            if (type2 >= 8 .and. type2 <= 32) type2 = 64
            if (type3 >= 8 .and. type3 <= 32) type3 = 64
            select case (op)
            case ('SDIV','SBSR','SBGE','SBRG','SBLE','SBRL','SBRC','SBNC')
                if (type2==64) then
                    call app('tmp2.v'//trim(typestr(type2))//'=('//stype2//')'//result2//';')
                    call app('tmp3.v'//trim(typestr(type3))//'=('//stype3//')'//result3//';')
                else
                    call app('tmp2.v'//trim(typestr(type2))//'='//result2//';')
                    call app('tmp3.v'//trim(typestr(type3))//'='//result3//';')
                end if
            case default
                call app('tmp2.v'//trim(typestr(type2))//'='//result2//';')
                call app('tmp3.v'//trim(typestr(type3))//'='//result3//';')
            end select
            select case (op)
            case ('ADD')
                call app('tmp1.v64=tmp2.v64+tmp3.v64;')
            case ('SUB')
                call app('tmp1.v64=tmp2.v64-tmp3.v64;')
            case ('MLT')
                call app('tmp1.v32=tmp2.v32*tmp3.v32;')
            case ('DIV')
                call app('tmp1.v32=tmp2.v32/tmp3.v32;')
            case ('MOD')
                call app('tmp1.v32=tmp2.v32%tmp3.v32;')
            case ('SDIV')
                call app('tmp1.v32=(int)tmp2.v32/(int)tmp3.v32;')
            case ('FADD')
                call app('tmp1.vREAL=tmp2.vREAL+tmp3.vREAL;')
            case ('FSUB')
                call app('tmp1.vREAL=tmp2.vREAL-tmp3.vREAL;')
            case ('FMLT')
                call app('tmp1.vREAL=tmp2.vREAL*tmp3.vREAL;')
            case ('FDIV')
                call app('tmp1.vREAL=tmp2.vREAL/tmp3.vREAL;')
            case ('FMOD')
                call app('tmp1.vREAL=fmod(tmp2.vREAL,tmp3.vREAL);')
            case ('LFADD')
                call app('tmp1.vLREAL=tmp2.vLREAL+tmp3.vLREAL;')
            case ('LFSUB')
                call app('tmp1.vLREAL=tmp2.vLREAL-tmp3.vLREAL;')
            case ('LFMLT')
                call app('tmp1.vLREAL=tmp2.vLREAL*tmp3.vLREAL;')
            case ('LFDIV')
                call app('tmp1.vLREAL=tmp2.vLREAL/tmp3.vLREAL;')
            case ('LFMOD')
                call app('tmp1.vLREAL=fmod(tmp2.vLREAL,tmp3.vLREAL);')
            case ('BSL')
                call app('tmp1.v32=tmp2.v32<<tmp3.v32;')
            case ('BSR')
                call app('tmp1.v32=tmp2.v32>>tmp3.v32;')
            case ('SBSR')
                call app('tmp1.v32=(int)tmp2.v32>>tmp3.v32;')
            case ('AND')
                call app('tmp1.v32=tmp2.v32&tmp3.v32;')
            case ('OR')
                call app('tmp1.v32=tmp2.v32|tmp3.v32;')
            case ('XOR')
                call app('tmp1.v32=tmp2.v32^tmp3.v32;')
            case ('NAND')
                call app('tmp1.v32=~(tmp2.v32&tmp3.v32);')
            case ('NOR')
                call app('tmp1.v32=~(tmp2.v32|tmp3.v32);')
            case ('XNOR')
                call app('tmp1.v32=~(tmp2.v32^tmp3.v32);')
            case ('BGE')
                call app('if (tmp2.v32>=tmp3.v32) goto *('//result1//');')
            case ('BRG')
                call app('if (tmp2.v32>tmp3.v32) goto *('//result1//');')
            case ('BLE')
                call app('if (tmp2.v32<=tmp3.v32) goto *('//result1//');')
            case ('BRL')
                call app('if (tmp2.v32<tmp3.v32) goto *('//result1//');')
            case ('BRE')
                call app('if (tmp2.v32==tmp3.v32) goto *('//result1//');')
            case ('BNE')
                call app('if (tmp2.v32!=tmp3.v32) goto *('//result1//');')
            case ('BRC')
                call app('if ((tmp2.v32+tmp3.v32)<tmp2.v32) goto *('//result1//');')
            case ('BNC')
                call app('if (!((tmp2.v32+tmp3.v32)<tmp2.v32)) goto *('//result1//');')
            case ('SBGE')
                call app('if ((int)tmp2.v32>=(int)tmp3.v32) goto *('//result1//');')
            case ('SBRG')
                call app('if ((int)tmp2.v32>(int)tmp3.v32) goto *('//result1//');')
            case ('SBLE')
                call app('if ((int)tmp2.v32<=(int)tmp3.v32) goto *('//result1//');')
            case ('SBRL')
                call app('if ((int)tmp2.v32<(int)tmp3.v32) goto *('//result1//');')
            case ('SBRC')
                call app('if ((int)tmp2.v32+tmp3.v32<(int)tmp2.v32) goto *('//result1//');')
            case ('SBNC')
                call app('if (!((int)tmp2.v32+tmp3.v32<(int)tmp2.v32)) goto *('//result1//');')
            case ('FBGE')
                call app('if (tmp2.vREAL>=tmp3.vREAL) goto *('//result1//');')
            case ('FBRG')
                call app('if (tmp2.vREAL>tmp3.vREAL) goto *('//result1//');')
            case ('FBLE')
                call app('if (tmp2.vREAL<=tmp3.vREAL) goto *('//result1//');')
            case ('FBRL')
                call app('if (tmp2.vREAL<tmp3.vREAL) goto *('//result1//');')
            case ('FBRE')
                call app('if (tmp2.vREAL==tmp3.vREAL) goto *('//result1//');')
            case ('FBNE')
                call app('if (tmp2.vREAL!=tmp3.vREAL) goto *('//result1//');')
            case ('LFBGE')
                call app('if (tmp2.vLREAL>=tmp3.vLREAL) goto *('//result1//');')
            case ('LFBRG')
                call app('if (tmp2.vLREAL>tmp3.vLREAL) goto *('//result1//');')
            case ('LFBLE')
                call app('if (tmp2.vLREAL<=tmp3.vLREAL) goto *('//result1//');')
            case ('LFBRL')
                call app('if (tmp2.vLREAL<tmp3.vLREAL) goto *('//result1//');')
            case ('LFBRE')
                call app('if (tmp2.vLREAL==tmp3.vLREAL) goto *('//result1//');')
            case ('LFBNE')
                call app('if (tmp2.vLREAL!=tmp3.vLREAL) goto *('//result1//');')
            case ('LLOD')
                if (.not.memdec) then
                    call throw('size of memory must be declared before use of LLOD')
                end if
                call app('tmp1.v'//typestr(type1)//'=*('//c_type(type1)//'*)((long long)tmp2.vADDR+tmp3.v64);')
            end select
            select case (op)
            case ('BGE','BRG','BLE','BRL','BRE','BNE','BRC','BNC','SBGE','SBRG','SBLE','SBRL','SBRC','SBNC','FBGE','FBRG','FBLE',&
                 &'FBRL','FBRE','FBNE','LFBGE','LFBRG','LFBLE','LFBRL','LFBRE','LFBNE')
            case default
                result2 = 'tmp1.v'//trim(typestr(type1))
                call vars(getvar_index(vars, result1))%set(result2)
            end select
        end if
    end subroutine

    subroutine standard2Op(op, arg1,arg2,vars,dws)
        type(DW), allocatable, intent(in) :: dws(:)
        type(variable), allocatable, intent(in) :: vars(:)
        character(len=:), allocatable, intent(in) :: arg1, arg2, op
        character(len=:), allocatable :: result1, result2, stype2
        character(len=:), allocatable :: output1, output2, output2u
        integer type1, type2
        result1 = parseArg(arg1, type1, vars, dws)
        result2 = parseArg(arg2, type2, vars, dws)
        select case (op)
        case ('BRP','BRN','BRZ','BNZ','FBRP','FBRN','FBRZ','FBNZ','LFBRP','LFBRN','LFBRZ','LFBNZ','BEV','BOD')
            if (type1/=1) then
                call throw('arg1 of '//op//' must be of type ADDR')
            end if
        case default
            if (result1(:1)/='V') then
                call throw('arg1 of '//op//' must be a variable')
                
            end if
        end select
        if (arch=='IRIS') then
            select case (op)
            case ('MOV','INC','LSH','NEG','NOT','ITOF','FTOI','RSH','FNEG','FABS')
                result1 = result1(2:)
                if (type1/=32) then
                    call parseSmall(output2,result2,3,vars)
                    select case (op)
                    case ('FNEG')
                        call vars(getvar_index(vars, result1))%set('NOT '//output2)
                    case default
                        call vars(getvar_index(vars, result1))%set(op//' '//output2)
                    end select
                else
                    call parseBig(output2,output2u,result2,3,vars,type2)
                    select case (op)
                    case ('MOV')
                        call vars(getvar_index(vars, result1))%set(' '//output2)
                        if (output2u/='') then
                            call vars(getvar_index(vars, result1))%set(' '//output2u,.true.)
                        else
                            call vars(getvar_index(vars, result1))%set(' R0',.true.)
                        end if
                    case ('INC')
                        if (result2(2:)/=result1(2:)) then
                            if (output2u=='') then
                                call vars(getvar_index(vars, result1))%set(' R0',.true.)
                            else
                                call vars(getvar_index(vars, result1))%set(' '//output2u,.true.)
                            end if
                        end if
                        call app('BNC .unique'//itoa(unique)//' '//output2//' 1')
                        if (output2u/='') then
                            call vars(getvar_index(vars, result1))%set('INC '//output2u,.true.)
                        else
                            call vars(getvar_index(vars, result1))%set(' 1',.true.)
                        end if
                        call app('.unique'//itoa(unique))
                        unique = unique + 1
                        call vars(getvar_index(vars, result1))%set('INC '//output2)
                    case ('LSH')
                        if (output2u/='') then
                            call app('LSH R25 '//output2u)
                        else
                            call app('MOV R25 R0')
                        end if
                        call app('BRP ~+2 '//output2)
                        call app('INC R25 R25')
                        call vars(getvar_index(vars, result1))%set('LSH '//output2)
                        call vars(getvar_index(vars, result1))%set(' R25',.true.)
                    case ('RSH')
                        if (output2u/='') then
                            call app('AND R25 1 '//output2u)
                            call app('BSL R25 R25 15')
                            call app('RSH R23 '//output2)
                            call vars(getvar_index(vars, result1))%set('ADD '//output2//' R25')
                            call vars(getvar_index(vars, result1))%set('RSH '//output2u,.true.)
                        else
                            call vars(getvar_index(vars, result1))%set('RSH '//output2)
                            call vars(getvar_index(vars, result1))%set(' 0',.true.)
                        end if
                    case ('NEG')
                        call app('NOT R23 '//output2)
                        if (output2u=='') then
                            call app('NOT R24 R0')
                        else
                            call app('NOT R24 '//output2u)
                        end if
                        call app('BNC ~+2 R23 1')
                        call app('INC R24 R24')
                        call vars(getvar_index(vars, result1))%set('INC R23')
                        call vars(getvar_index(vars, result1))%set(' R24',.true.)
                    case ('NOT')
                        call vars(getvar_index(vars, result1))%set('NOT '//output2)
                        call vars(getvar_index(vars, result1))%set('NOT '//output2u,.true.)
                    case ('ITOF')
                        call vars(getvar_index(vars, result1))%set(op//' '//output2)
                    case ('FTOI')
                        call vars(getvar_index(vars, result1))%set(op//' '//output2)
                        call vars(getvar_index(vars, result1))%set(' R0',.true.)
                    case default
                        call throw('32 bit translation not implemented for '//op,.false.)
                    end select
                end if
            case ('FBRP','BRP','BRN','BRZ','BNZ','BEV','BOD')
                call parseSmall(output1,result1,2,vars)
                if (type2/=32) then
                    call parseSmall(output2,result2,3,vars)
                    select case (op)
                    case ('FBRP')
                        call app('BRP '//output1//' '//output2)
                    case default
                        call app(op//' '//output1//' '//output2)
                    end select
                else
                    call parseBig(output2,output2u,result2,3,vars,type2)
                    select case (op)
                    case ('BRP')
                        call app('BRP '//output1//' '//output2u)
                    case ('BRN')
                        call app('BRN '//output1//' '//output2u)
                    case ('BEV')
                        call app('BEV '//output1//' '//output2)
                    case ('BOD')
                        call app('BOD '//output1//' '//output2)
                    case ('BNZ')
                        call app('BNZ '//output1//' '//output2)
                        call app('BNZ '//output1//' '//output2u)
                    case ('BRZ')
                        call app('BNZ ~+2 '//output2u)
                        call app('BRZ '//output1//' '//output2)
                    case default
                        call throw('32 bit translation not implemented for '//op,.false.)
                    end select
                end if
            case default
                call throw('unknown instruction '//op//' for arch IRIS')
            end select
        else if (arch(:1)=='C') then
            result1 = result1(2:)
            result2 = result2(2:)
            stype2 = signed_c_type(type2)
            if (type2 >= 8 .and. type2 <= 32) type2 = 32
            select case(op)
            case ('SMOV','SRSH','BRP','BRN')
                if (type2 == 32) call app('tmp2.v'//trim(typestr(type2))//'=('//stype2//')'//result2//';')
                if (type2 /= 32) call app('tmp2.v'//trim(typestr(type2))//'='//result2//';')
            case default
                call app('tmp2.v'//trim(typestr(type2))//'='//result2//';')
            end select
            select case(op)
            case ('MOV','SMOV')
                call app('tmp1.v64=tmp2.v64;')
            case ('ABS')
                call app('tmp1.v32=abs((int)tmp2.v32);')
            case ('NEG')
                call app('tmp1.v32=-tmp2.v32;')
            case ('LSH')
                call app('tmp1.v32=tmp2.v32<<1;')
            case ('RSH')
                call app('tmp1.v32=tmp2.v32>>1;')
            case ('SRSH')
                call app('tmp1.v32=(int)tmp2.v32>>1;')
            case ('INC')
                call app('tmp1.v32=tmp2.v32+1;')
            case ('DEC')
                call app('tmp1.v32=tmp2.v32-1;')
            case ('NOT')
                call app('tmp1.v32=~tmp2.v32;')
            case ('BRP')
                call app('if ((int)tmp2.v32>=0) goto *('//result1//');')
            case ('BRN')
                call app('if ((int)tmp2.v32<0) goto *('//result1//');')
            case ('BRZ')
                call app('if (tmp2.v32==0) goto *('//result1//');')
            case ('BNZ')
                call app('if (tmp2.v32!=0) goto *('//result1//');')
            case ('BEV')
                call app('if (!(tmp2.v32&1)) goto *('//result1//');')
            case ('BOD')
                call app('if (tmp2.v32&1) goto *('//result1//');')
            case ('FABS')
                call app('tmp1.vREAL=fabs(tmp2.vREAL);')
            case ('FNEG')
                call app('tmp1.vREAL=-tmp2.vREAL;')
            case ('FBRP')
                call app('if (tmp2.vREAL>=0) goto *('//result1//');')
            case ('FBRN')
                call app('if (tmp2.vREAL<0) goto *('//result1//');')
            case ('FBRZ')
                call app('if (tmp2.vREAL==0) goto *('//result1//');')
            case ('FBNZ')
                call app('if (tmp2.vREAL!=0) goto *('//result1//');')
            case ('LFBRP')
                call app('if (tmp2.vLREAL>=0) goto *('//result1//');')
            case ('LFBRN')
                call app('if (tmp2.vLREAL<0) goto *('//result1//');')
            case ('LFBRZ')
                call app('if (tmp2.vLREAL==0) goto *('//result1//');')
            case ('LFBNZ')
                call app('if (tmp2.vLREAL!=0) goto *('//result1//');')
            case ('ITOF')
                call app('tmp1.vREAL=(int)tmp2.v32;')
            case ('FTOI')
                call app('tmp1.v32=(int)tmp2.vREAL;')
            case ('ITOLF')
                call app('tmp1.vLREAL=(int)tmp2.v32;')
            case ('LFTOI')
                call app('tmp1.v32=(int)tmp2.vLREAL;')
            case ('FTOLF')
                call app('tmp1.vLREAL=tmp2.vREAL;')
            case ('LFTOF')
                call app('tmp1.vREAL=(float)tmp2.vLREAL;')
            end select
            result2 = 'tmp1.v'//trim(typestr(type1))
            select case (op)
            case ('BRP','BRN','BRZ','BNZ','FBRP','FBRN','FBRZ','FBNZ','LFBRP','LFBRN','LFBRZ','LFBNZ','BEV','BOD')
            case default
                call vars(getvar_index(vars, result1))%set(result2)
            end select
        end if
    end subroutine

    subroutine standard1Op(op,arg1,vars,dws)
        type(DW), allocatable, intent(in) :: dws(:)
        type(variable), allocatable, intent(in) :: vars(:)
        character(len=:), allocatable, intent(in) :: arg1, op
        character(len=:), allocatable :: result1, output1, output1u
        integer type1
        integer tmp
        result1 = parseArg(arg1, type1, vars, dws)
        select case (op)
        case ('CAL')
            if (type1/=1) then
                call throw('arg1 of '//op//' must be of type ADDR')
            end if
            if (.not.cstackdec) then
                call throw('size of call stack must be declared before use of CAL')
            end if
        case ('JMP')
            if (type1/=1) then
                call throw('arg1 of '//op//' must be of type ADDR')
            end if
        case ('PSH')
            if (.not.stackdec) then
                call throw('size of stack must be declared before use of PSH')
            end if
        case ('POP')
            if (result1(:1)/='V') then
                call throw('arg1 of POP must be a variable')
            end if
            if (.not.stackdec) then
                call throw('size of stack must be declared before use of POP')
            end if
        end select
        if (arch(:1)=='C') then
            result1 = result1(2:)
            select case (op)
            case ('JMP')
                call app('goto *('//result1//');')
            case ('CAL')
                call app('cstack[csp]=&&line'//itoa(id)//'_'//itoa(lnum2+1)//';')
                call app('csp++;')
                call app('goto *('//result1//');')
            case ('PSH')
                tmp = memsze
                if (typesize(type1)>typesize(tmp)) tmp = type1
                call app('*('//c_type(tmp)//'*)sp='//result1//';')
                call app('sp+=1+(sizeof('//c_type(tmp)//')-1)/sizeof('//c_type(memsze)//');')
            case ('POP')
                tmp = memsze
                if (typesize(type1)>typesize(tmp)) tmp = type1
                call app('sp-=1+(sizeof('//c_type(tmp)//')-1)/sizeof('//c_type(memsze)//');')
                call app(result1//'=*('//c_type(tmp)//'*)sp;')
            end select
        else if (arch=='IRIS') then
            select case (op)
            case ('PSH')
                if (type1/=32) then
                    call parseSmall(output1,result1,2,vars)
                    call app('HPSH '//output1)
                else
                    call parseBig(output1,output1u,result1,2,vars,type1)
                    call app('HPSH '//output1)
                    call app('HPSH '//output1u)
                end if
            case ('POP')
                result1 = result1(2:)
                if (type1/=32) then
                    call vars(getvar_index(vars, result1))%set('HPOP ')
                else
                    call vars(getvar_index(vars, result1))%set('HPOP ',.true.)
                    call vars(getvar_index(vars, result1))%set('HPOP ')
                end if
            case ('CAL')
                call parseSmall(output1,result1,2,vars)
                call app('HCAL '//output1)
            case ('JMP')
                call parseSmall(output1,result1,2,vars)
                call app('JMP '//output1)
            case default
                call throw('unknown instruction '//op//' for arch IRIS')
            end select
        end if
    end subroutine

    subroutine str(arg1,arg2,vars,dws)
        type(DW), allocatable, intent(in) :: dws(:)
        type(variable), allocatable, intent(in) :: vars(:)
        character(len=:), allocatable, intent(in) :: arg1, arg2
        character(len=:), allocatable :: result1, result2
        character(len=:), allocatable :: output1, output2, output2u
        integer type1, type2
        if (.not.memdec) then
            call throw('size of memory must be declared before use of STR')
        end if
        result1 = parseArg(arg1, type1, vars, dws)
        result2 = parseArg(arg2, type2, vars, dws)
        if (type1/=1) then
            call throw('arg1 of STR must be of size ADDR')
        end if
        if (arch(:1)=='C') then
            result1 = result1(2:)
            result2 = result2(2:)
            call app('*('//c_type(type2)//'*)'//result1//'='//result2//';')
        else if (arch=='IRIS') then
            call parseSmall(output1,result1,1,vars)
            output2 = result2(:1) ! var missused here
            result2 = result2(2:)
            if (output2=='V') then
                if (vars(getvar_index(vars, result2))%location>=maxregplus1) then
                    call app('CPY '//output1//' '//itoa(vars(getvar_index(vars, result2))%location))
                    if (type2==32) then
                        call app('INC R25 '//output1)
                        call app('CPY R25 '//itoa(vars(getvar_index(vars, result2))%location+1))
                    end if
                    return
                end if
            end if
            result2 = output2//result2
            call parseBig(output2,output2u,result2,2,vars,type2)
            call app('STR '//output1//' '//output2)
            if (type2==32) then
                call app('LSTR '//output1//' 1 '//output2u)
            end if
        end if
    end subroutine

    subroutine lstr(arg1,arg2,arg3,vars,dws)
        type(DW), allocatable, intent(in) :: dws(:)
        type(variable), allocatable, intent(in) :: vars(:)
        character(len=:), allocatable, intent(in) :: arg1, arg2, arg3
        character(len=:), allocatable :: result1, result2, result3
        character(len=:), allocatable :: output1, output2, output3, output3u
        integer type1, type2, type3, itemp, itemp2
        if (.not.memdec) then
            call throw('size of memory must be declared before use of LSTR')
        end if
        result1 = parseArg(arg1, type1, vars, dws)
        result2 = parseArg(arg2, type2, vars, dws)
        result3 = parseArg(arg3, type3, vars, dws)
        if (type1/=1) then
            call throw('arg1 of LSTR must be of type ADDR')
        end if
        if (arch(:1)=='C') then
            result1 = result1(2:)
            result2 = result2(2:)
            result3 = result3(2:)
            if (type1 >= 8 .and. type1 <= 32) type1 = 64
            if (type2 >= 8 .and. type2 <= 32) type2 = 64
            call app('tmp1.v'//trim(typestr(type1))//'='//result1//';')
            call app('tmp2.v'//trim(typestr(type2))//'='//result2//';')
            call app('*('//c_type(type3)//'*)((long long)tmp1.vADDR+tmp2.v64)='//result3//';')
        else if (arch=='IRIS') then
            itemp = iachar(result2(:1))
            itemp2 = iachar(result2(2:2))
            call parseSmall(output1,result1,1,vars)
            call parseSmall(output2,result2,2,vars)
            call parseBig(output3,output3u,result3,3,vars,type3)
            call app('LSTR '//output1//' '//output2//' '//output3)
            if (type3==32) then
                if (achar(itemp)=='V'.or.achar(itemp2)=='.'.or.achar(itemp2)=='M') then
                    call app('INC R25 '//output2)
                    call app('LSTR '//output1//' R25 '//output3u)
                else
                    read(output2,*) itemp
                    call app('LSTR '//output1//' '//itoa(itemp+1)//' '//output3u)
                end if
            end if
        end if
    end subroutine

    subroutine cpy(arg1,arg2,arg3,vars,dws)
        type(DW), allocatable, intent(in) :: dws(:)
        type(variable), allocatable, intent(in) :: vars(:)
        character(len=:), allocatable, intent(in) :: arg1, arg2, arg3
        character(len=:), allocatable :: result1, result2, result3
        integer type1, type2, type3
        if (.not.memdec) then
            call throw('size of memory must be declared before use of CPY')
            
        end if
        result1 = parseArg(arg1, type1, vars, dws)
        result2 = parseArg(arg2, type2, vars, dws)
        result3 = parseArg(arg3, type3, vars, dws)
        if (type1/=1) then
            call throw('arg1 of CPY must be of type ADDR')
            
        end if
        if (type2/=1) then
            call throw('arg2 of CPY must be of type ADDR')
            
        end if
        if (arch(:1)=='C') then
            result1 = result1(2:)
            result2 = result2(2:)
            result3 = result3(2:)
            if (type1 >= 8 .and. type1 <= 32) type2 = 32
            if (type2 >= 8 .and. type2 <= 32) type2 = 32
            if (type3 >= 8 .and. type3 <= 32) type3 = 32
            call app('tmp1.v'//trim(typestr(type2))//'='//result2//';')
            call app('tmp2.v'//trim(typestr(type2))//'='//result2//';')
            call app('tmp3.v'//trim(typestr(type3))//'='//result3//';')
            call app('memcpy(tmp1.vADDR, tmp2.vADDR, tmp3.v32);')
        end if
    end subroutine

    subroutine lod(arg1,arg2,vars,dws)
        type(DW), allocatable, intent(in) :: dws(:)
        type(variable), allocatable, intent(in) :: vars(:)
        character(len=:), allocatable, intent(in) :: arg1, arg2
        character(len=:), allocatable :: result1, result2, output2
        integer type1, type2
        if (.not.memdec) then
            call throw('size of memory must be declared before use of LOD')
            
        end if
        result1 = parseArg(arg1, type1, vars, dws)
        result2 = parseArg(arg2, type2, vars, dws)
        if (type2/=1) then
            call throw('arg2 of LOD must be of size ADDR')
            
        end if

        result1 = result1(2:)
        if (arch(:1)=='C') then
            result2 = result2(2:)
            call app(result1//'=*('//c_type(type1)//'*)'//result2//';')
        else if (arch=='IRIS') then
            call parseSmall(output2,result2,2,vars)
            call vars(getvar_index(vars, result1))%set('LOD '//output2)
            if (type1==32) call vars(getvar_index(vars, result1))%set('LLOD 1 '//output2,.true.)
        end if
    end subroutine

    subroutine ret()
        if (.not.cstackdec) then
            call throw('size of call stack must be declared before use of RET')
            
        end if
        if (arch(:1)=='C') then
            call app('csp--;')
            call app('goto *cstack[csp];')
        else if (arch=='IRIS') then
            call app('HRET')
        end if
    end subroutine

    subroutine out(arg1,arg2,arg3,arg4,vars,dws)
        type(DW), allocatable, intent(in) :: dws(:)
        type(variable), allocatable, intent(in) :: vars(:)
        character(len=:), allocatable, intent(in) :: arg1, arg2, arg3, arg4
        character(len=:), allocatable :: result1, result2, output2, output2u
        character(len=:), allocatable :: result3, result4, output3, output4
        integer type1, type2, itemp
        result1 = parseArg(arg1, type1, vars, dws)
        result2 = parseArg(arg2, type2, vars, dws)
        if (type1/=4) then
            call throw('arg1 of OUT must be a port')
            
        end if
        if (arch(:1)=='C') then
            result2 = result2(2:)
            select case (result1)
            case ('%TEXT','%ASCII')
                call app('printf("%c",'//result2//');')
            case ('%NUMB')
                itemp = type2
                if (itemp>=8) itemp = 32
                call app('tmp1.v'//typestr(itemp)//' = '//result2//';')
                call app('printf("%d",tmp1.v32);')
            case ('%FLOAT')
                call app('printf("%f",'//result2//');')
            case ('%CLEAR')
                call app('if (buffered) write = data2; else write = data;')
                call app('memset(write,0,height*width*4);')
            case ('%BUFFER')
                call app('buffered = 1;')
                call app('mtx_lock(mtxptr);')
                call app('data2 = malloc(width*height*4);')
                call app('width2=width; height2=height;')
                call app('memcpy(data2,data,width*height*4);')
                call app('mtx_unlock(mtxptr);')
            case ('%UNBUFFER')
                call app('buffered = 0;')
                call app('mtx_lock(mtxptr);')
                call app('memcpy(data,data2,fmin(width,width2)*fmin(height,height2)*4);')
                call app('mtx_unlock(mtxptr);')
                call app('free(data2);')
            case ('%PIXEL')
                result3 = parseArg(arg3, type1, vars, dws)
                result4 = parseArg(arg4, type1, vars, dws)
                result3 = result3(2:)
                result4 = result4(2:)
                call app('mtx_lock(mtxptr);')
                itemp = type1
                if (itemp>=8) itemp = 32
                call app('tmp1.v'//typestr(itemp)//' = '//result4//';')
                call app('if (buffered) write = data2; else write = data;')
                select case (colormode)
                case ('BIN')
                    call app('write[((buffered ? height2 : height)-'//result3//')*(buffered ? width2 : width)*4+'//result2&
                     //'*4] = tmp1.v32>=1;')
                    call app('write[((buffered ? height2 : height)-'//result3//')*(buffered ? width2 : width)*4+'//result2&
                     //'*4+1] = tmp1.v32>=1;')
                    call app('write[((buffered ? height2 : height)-'//result3//')*(buffered ? width2 : width)*4+'//result2&
                     //'*4+2] = tmp1.v32>=1;')
                case ('MONO')
                    call app('write[((buffered ? height2 : height)-'//result3//')*(buffered ? width2 : width)*4+'//result2&
                     //'*4] = tmp1.v32;')
                    call app('write[((buffered ? height2 : height)-'//result3//')*(buffered ? width2 : width)*4+'//result2&
                     //'*4+1] = tmp1.v32;')
                    call app('write[((buffered ? height2 : height)-'//result3//')*(buffered ? width2 : width)*4+'//result2&
                     //'*4+2] = tmp1.v32;')
                case ('RGB8')
                    call app('write[((buffered ? height2 : height)-'//result3//')*(buffered ? width2 : width)*4+'//result2&
                     //'*4] = ((tmp1.v32&0xE0)>>5)*36;')
                    call app('write[((buffered ? height2 : height)-'//result3//')*(buffered ? width2 : width)*4+'//result2&
                     //'*4+1] = (tmp1.v32&0x1C)*9;')
                    call app('write[((buffered ? height2 : height)-'//result3//')*(buffered ? width2 : width)*4+'//result2&
                     //'*4+2] = (tmp1.v32&3)*85;')
                case ('RGB24')
                    call app('write[((buffered ? height2 : height)-'//result3//')*(buffered ? width2 : width)*4+'//result2&
                     //'*4] = tmp1.v32>>16;')
                    call app('write[((buffered ? height2 : height)-'//result3//')*(buffered ? width2 : width)*4+'//result2&
                     //'*4+1] = (tmp1.v32>>8)&0xFF;')
                    call app('write[((buffered ? height2 : height)-'//result3//')*(buffered ? width2 : width)*4+'//result2&
                     //'*4+2] = tmp1.v32&0xFF;')
                end select
                call app('mtx_unlock(mtxptr);')
                incopengl = .true.
            case default
                call throw('unknown port "'//result1//'" for target C')
            end select
        else if (arch=='IRIS') then
            select case (result1)
            case ('%TEXT')
                call parseSmall(output2,result2,2,vars)
                call app('OUT %TEXT '//output2)
            case ('%NUMB')
                if (type2/=32) then
                    call parseSmall(output2,result2,2,vars)
                    call app('OUT %NUMB '//output2)
                else
                    call parseBig(output2,output2u,result2,2,vars,type2)
                    incprint32 = .true.
                    if (output2(:1)=='R') then
                        call app('BNZ ~+3 '//output2u//achar(10)//'OUT %NUMB '//output2)
                        if (output2=='R1') then
                            call app('JMP ~+6'//achar(10)//'HPSH R2'//achar(10)//'HPSH R1'//achar(10)//&
                             'HCAL .print32'//achar(10)//'HPOP R1'//achar(10)//'HPOP R2')
                        else
                            call app('JMP ~+8'//achar(10)//'HPSH R2'//achar(10)//'HPSH R1')
                            call app('MOV R1 '//output2//achar(10)//'MOV R2 '//output2u)
                            call app('HCAL .print32'//achar(10)//'HPOP R1'//achar(10)//'HPOP R2')
                        end if
                    else
                        call app('HPSH R2'//achar(10)//'HPSH R1')
                        call app('IMM R1 '//output2//achar(10)//'IMM R2 '//output2u)
                        call app('HCAL .print32'//achar(10)//'HPOP R1'//achar(10)//'HPOP R2')
                    end if
                end if
            case ('%PIXEL')
                result3 = parseArg(arg3, type1, vars, dws)
                result4 = parseArg(arg4, type1, vars, dws)
                call parseSmall(output2,result2,1,vars)
                call parseSmall(output3,result3,2,vars)
                call parseSmall(output4,result4,3,vars)
                call app('OUT %X '//output2//achar(10)//'OUT %Y '//output3)
                call app('OUT %COLOR '//output4)
            case ('%BUFFER')
                if (urcx) then
                    call app('OUT %BUFFER 1')
                else 
                    call app('OUT %FREEZE 1')
                end if
            case ('%UNBUFFER')
                if (urcx) then
                    call app('OUT %BUFFER 0')
                    call throw('warning: %unbuffer translation is not exactly correct for urcx',.false.)
                else
                    call app('OUT %UNFREEZE 0')
                end if
            case ('%CLEAR')
                if (urcx) then
                    call app('IN R25 %X')
                    call app('IN R24 %Y')
                    call app('MOV R23 R0')
                    call app('MOV R22 R0')
                    call app('OUT %X R22')
                    call app('OUT %Y R23')
                    call app('OUT %COLOR 0')
                    call app('INC R22 R22')
                    call app('BRL ~-4 R22 R25')
                    call app('INC R23 R23')
                    call app('BRL ~-7 R23 R24')
                    call throw('warning: %clear translation is suboptimal for urcx',.false.)
                else
                    call app('OUT %CLEAR 0')
                end if
            case default
                call throw('unknown port "'//result1//'" for target IRIS')
            end select
        end if

    end subroutine

    subroutine in(arg1,arg2,arg3,arg4,vars,dws)
        type(DW), allocatable, intent(in) :: dws(:)
        type(variable), allocatable, intent(in) :: vars(:)
        character(len=:), allocatable, intent(in) :: arg1, arg2, arg3, arg4
        character(len=:), allocatable :: result1, result2, result3, result4
        character(len=:), allocatable :: output1, output3, output4
        integer type1, type2
        result1 = parseArg(arg1, type1, vars, dws)
        result2 = parseArg(arg2, type2, vars, dws)
        if (type2/=4) then
            call throw('arg2 of IN must be a port')
            
        end if
        if (arch(:1)=='C') then
            result1 = result1(2:)
            select case (result2)
            case ('%TEXT','%ASCII')
                call app('tmp1.v32=getch();')
                result2 = 'tmp1.v'//trim(typestr(type1))
                call vars(getvar_index(vars, result1))%set(result2)
            case ('%TIME')
                call app('tmp1.vREAL=glfwGetTime();')
                result2 = 'tmp1.v'//trim(typestr(type1))
                call vars(getvar_index(vars, result1))%set(result2)
            case ('%SIZEX')
                call app('mtx_lock(mtxptr);')
                call app('tmp1.v32=buffered ? width2 : width;')
                call app('mtx_unlock(mtxptr);')
                result2 = 'tmp1.v'//trim(typestr(type1))
                call vars(getvar_index(vars, result1))%set(result2)
                incopengl = .true.
            case ('%SIZEY')
                call app('mtx_lock(mtxptr);')
                call app('tmp1.v32=buffered ? height2 : height;')
                call app('mtx_unlock(mtxptr);')
                result2 = 'tmp1.v'//trim(typestr(type1))
                call vars(getvar_index(vars, result1))%set(result2)
                incopengl = .true.
            case ('%MOUSE_POSX')
                call app('{')
                call app('double tmpx, tmpy;')
                call app('mtx_lock(mtxptr);')
                call app('glfwGetCursorPos((GLFWwindow*)windowptr, &tmpx, &tmpy);')
                call app('tmp1.v32=tmpx*2>=(buffered ? width2 : width) ? (buffered ? width2 : width) : tmpx*2;')
                call app('mtx_unlock(mtxptr);')
                call app('}')
                result2 = 'tmp1.v'//trim(typestr(type1))
                call vars(getvar_index(vars, result1))%set(result2)
                incopengl = .true.
            case ('%MOUSE_POSY')
                call app('{')
                call app('double tmpx, tmpy;')
                call app('mtx_lock(mtxptr);')
                call app('glfwGetCursorPos((GLFWwindow*)windowptr, &tmpx, &tmpy);')
                call app('tmp1.v32=tmpy*2>=(buffered ? height2 : height) ? (buffered ? height2 : height) : tmpy*2;')
                call app('mtx_unlock(mtxptr);')
                call app('}')
                result2 = 'tmp1.v'//trim(typestr(type1))
                call vars(getvar_index(vars, result1))%set(result2)
                incopengl = .true.
            case ('%EXIT')
                call app('mtx_lock(mtxptr);'//achar(10)//'tmp1.v32=status;'//achar(10)//'mtx_unlock(mtxptr);')
                result2 = 'tmp1.v'//trim(typestr(type1))
                call vars(getvar_index(vars, result1))%set(result2)
                incopengl = .true.
            case ('%PIXEL')
                result3 = parseArg(arg3, type1, vars, dws)
                result4 = parseArg(arg4, type1, vars, dws)
                result3 = result3(2:)
                result4 = result4(2:)
                call app('mtx_lock(mtxptr);')
                call app('if (buffered) write = data2; else write = data;')
                select case (colormode)
                case ('BIN')
                    call app(result1//' = write[(height-'//result3//')*width*4+'//result4//'*4];')
                case ('MONO')
                    call app(result1//' = write[(height-'//result3//')*width*4+'//result4//'*4];')
                case ('RGB8')
                    call app(result1//' = write[(height-'//result3//')*width*4+'//result4//'*4+2]/85+'//&
                    'write[(height-'//result3//')*width*4+'//result4//'*4+1]/9+'//&
                    '(write[(height-'//result3//')*width*4+'//result4//'*4+2]/36<<5);')
                end select
                call app('mtx_unlock(mtxptr);')
                incopengl = .true.
            case default
                call throw('unknown port "'//result2//'" for target C')
            end select
        else if (arch=='IRIS') then
            result1 = result1(2:)
            select case (result2)
            case ('%TEXT')
                call vars(getvar_index(vars, result1))%set('IN %TEXT')
            case ('%TIME')
                if (urcx) then
                    call app('IN R25 %TIME')
                    call app('BSR R25 R25 6')
                    call app('ITOF R25 R25')
                    call vars(getvar_index(vars, result1))%set('FMLT R25 0.064')
                else
                    call throw('%TIME only exists in urcx for target IRIS')
                end if
            case ('%MOUSE_POSX')
                if (urcx) then
                    call vars(getvar_index(vars, result1))%set('IN %MOUSE_X')
                else
                    call throw('%MOUSE_POSX only exists in urcx for target IRIS')
                end if
            case ('%MOUSE_POSY')
                if (urcx) then
                    call vars(getvar_index(vars, result1))%set('IN %MOUSE_Y')
                else
                    call throw('%MOUSE_POSY only exists in urcx for target IRIS')
                end if
            case ('%SIZEX')
                call vars(getvar_index(vars, result1))%set('IN %X')
            case ('%SIZEY')
                call vars(getvar_index(vars, result1))%set('IN %Y')
            case ('%EXIT')
                call vars(getvar_index(vars, result1))%set(' R0')
            case ('%PIXEL')
                result3 = parseArg(arg3, type1, vars, dws)
                result4 = parseArg(arg4, type1, vars, dws)
                call parseSmall(output1,result1,1,vars)
                call parseSmall(output3,result3,2,vars)
                call parseSmall(output4,result4,3,vars)
                call app('OUT %X '//output3//achar(10)//'OUT %Y '//output4)
                call app('IN %COLOR '//output1)
            case default
                call throw('unknown port "'//result2//'" for target IRIS')
            end select
        end if
    end subroutine

    subroutine data(op,line,vars,dws)
        type(DW), allocatable, intent(in) :: dws(:)
        type(variable), allocatable, intent(in) :: vars(:)
        character(len=:), allocatable, intent(in) :: line
        character(len=:), allocatable, intent(inout) :: op
        character(len=:), allocatable :: result1, arg
        integer typedw, type1, temp, i, j
        logical skip
        op = op(2:)
        if (op=='W') then
            typedw = memsze
        else
            typedw = strtype(op)
        end if
        i = 1
        arg = getop(line,i,.false.)
        do while (arg/='')
            if (arg(:1)=='"') then
                skip = .false.
                do j=2,len(arg)-1
                    if (skip) then
                        skip = .false.
                        cycle
                    else if (arg(j:j)=='\') then
                        call app('DW '''//arg(j:j+1)//'''')
                        skip = .true.
                    else
                        call app('DW '//itoa(iachar(arg(j:j))))
                    end if
                    if (typedw==32) call app('DW 0')
                end do
            else
                result1 = parseArg(arg,type1,vars,dws)
                if (typedw/=32) then
                    call app('DW '//result1(2:))
                else
                    if (result1(2:3)=='0x') then
                        read(result1(4:), '(Z10)') temp
                    else
                        read(result1(2:),*) temp
                    end if
                    call app('DW '//itoa(iand(temp,2**16-1)))
                    call app('DW '//itoa(shiftr(temp,16)))
                end if
            end if
            i = i + 1
            arg = getop(line,i,.false.)
        end do
    end subroutine

    function silk_parseDws(count,dwlist,pass,input,inputDws) result(dws)
        type(DW), allocatable :: dws(:)
        integer, intent(out) :: count
        character(len=:), allocatable, intent(out) :: dwlist
        integer, intent(in) :: pass
        type(string), allocatable, intent(in), optional :: input(:)
        type(DW), allocatable, intent(in), optional :: inputDws(:)
        type(string), allocatable :: inputActual(:)
        character(len=:), allocatable :: dwmemsze, line, prevLine, temp
        logical comment, skip
        integer i,j,unused
        type(variable), allocatable :: vars(:)
        integer :: sizedw, type, memszet
        logical :: done, dwlabel, ininst
        if (pass==1) then
            allocate(vars(0))
        end if
        dwlabel = .false.
        count = 0
        dwmemsze = ''
        comment = .false.
        dwlist = ''
        prevLine = '' !to shut up gfortran
        ininst = .false.
        if (present(input)) then
            inputActual = input
            dwmemsze = typestr(memsze)
            memszet = memsze
        end if
        if (present(inputDws)) then
            dws = inputDws
        else
            allocate(dws(0))
        end if
        do
        if (present(input)) then
            if (allocated(inputActual)) then
                line = inputActual(1)%value
                if (size(inputActual)==1) then
                    deallocate(inputActual)
                else
                    inputActual = inputActual(2:)
                end if
            else
                dws = dws
                return
            end if
        else
            line = getline(done)
            if (done) return
        end if

        call fixstr(line,comment)
        if (getop(line,0,.false.)=='@ENDINST') then
            ininst = .false.
            cycle
        else if (ininst) then
            cycle
        else if (getop(line,0,.false.)=='@INST') then
            ininst = .true.
            cycle
        end if
        if (.not.present(input).and.line(:7)=='@MEMSZE ') then
            memszet = strtype(getop(line,1))
            dwmemsze = c_type(memszet)
        else if (pass==1) then
            if (line(:1)=='.') then
                dwlabel = .true.
                prevLine = line
            else if (line(:3)=='DW '.or.line(:3)=='D8 '.or.line(:4)=='D16 '.or.line(:4)=='D32 '.or.line(:6)=='DADDR '.or.&
                & line(:6)=='DREAL '.or.line(:7)=='DLREAL ') then
                
            else if (getop(line,0,.false.)/='') then
                dwlabel = .false.
            end if
        end if
        if (line(:3)=='DW '.or.line(:3)=='D8 '.or.line(:4)=='D16 '.or.line(:4)=='D32 '.or.line(:6)=='DADDR '.or.&
        & line(:6)=='DREAL '.or.line(:7)=='DLREAL ') then
            if (dwmemsze=='') then
                call throw('memsize must be declared before dw statments are used')
                
            end if
            if (dwlabel) then
                if (.not.allocated(dws)) then
                    allocate(dws(1))
                    dws(1) = dw(prevLine,count)
                else
                    dws = [dws, dw(prevLine,count)]
                end if
            end if
            if (line(2:2)=='W') then
                sizedw = 1
                type = memszet
            else
                temp = line(2:index(line,' ')-1)
                type = strtype(temp)
                sizedw = ceiling(typesize(type)/real(typesize(memszet)))
            end if
            i = 1
          5 if (line(i:i)=='['.or.line(i:i)==']') line = line(:i-1)//line(i+1:)
            i = i + 1
            if (i<=len(line)) goto 5
            j = 1
            do while (getop(line,j,.false.)/='')
                temp = getop(line,j)
                if (temp(:1)=='"') then
                    skip = .false.
                    temp = temp(2:len(trim(temp))-1)
                    do i=1,len(temp)
                        if (skip) then
                            skip = .false.
                            cycle
                        else if (temp(i:i)=='"') then
                            exit
                        else if (temp(i:i)=='\') then
                            dwlist = dwlist//' '''//temp(i:i+1)//''''
                            skip = .true.
                            if (sizedw==32) then
                                dwlist = dwlist//' 0'
                            end if
                        else
                            dwlist = dwlist//' '''//temp(i:i)//''''
                            if (sizedw==32) then
                                dwlist = dwlist//' 0'
                            end if
                        end if
                        count = count + sizedw
                    end do
                    count = count - sizedw
                else
                    temp = parseArg(temp,unused,vars,dws)
                    if (unused==32) then
                        read(temp(2:),*) unused
                        dwlist = dwlist//' '//itoa(iand(unused,65535))//' '//itoa(unused/65536)
                    else
                        dwlist = dwlist//' '//temp(2:)
                    end if
                end if
                j = j + 1
                count = count + sizedw
            end do
        end if
        call updatecom(line, comment)
        end do
    end function

    function c_parseDws(count,dwlist,pass,input,inputDws) result(dws)
        type(DW), allocatable :: dws(:)
        integer, intent(out) :: count
        character(len=:), allocatable, intent(out) :: dwlist
        integer, intent(in) :: pass
        type(string), allocatable, intent(in), optional :: input(:)
        type(DW), allocatable, intent(in), optional :: inputDws(:)
        type(string), allocatable :: inputActual(:)
        character(len=:), allocatable :: dwmemsze, line, prevLine, temp, temp2
        logical comment, skip
        integer i,unused,k,l
        type(variable), allocatable :: vars(:)
        integer :: sizedw, type, memszet
        logical :: done, dwlabel, ininst
        if (pass==1) then
            allocate(vars(0))
        end if
        dwlabel = .false.
        count = 0
        dwmemsze = ''
        comment = .false.
        dwlist = ''
        prevLine = '' !to shut up gfortran
        ininst = .false.
        if (present(input)) then
            inputActual = input
            dwmemsze = typestr(memsze)
            memszet = memsze
        end if
        if (present(inputDws)) then
            dws = inputDws
        else
            allocate(dws(0))
        end if
        do
        if (present(input)) then
            if (allocated(inputActual)) then
                line = inputActual(1)%value
                if (size(inputActual)==1) then
                    deallocate(inputActual)
                else
                    inputActual = inputActual(2:)
                end if
            else
                dws = dws
                return
            end if
        else
            line = getline(done)
            if (done) return
        end if

        call fixstr(line,comment)
        if (getop(line,0,.false.)=='@ENDINST') then
            ininst = .false.
            cycle
        else if (ininst) then
            cycle
        else if (getop(line,0,.false.)=='@INST') then
            ininst = .true.
            cycle
        end if
        if (.not.present(input).and.line(:7)=='@MEMSZE ') then
            memszet = strtype(getop(line,1))
            dwmemsze = c_type(memszet)
        else if (pass==1) then
            if (line(:1)=='.') then
                dwlabel = .true.
                prevLine = line
            else if (line(:3)=='DW '.or.line(:3)=='D8 '.or.line(:4)=='D16 '.or.line(:4)=='D32 '.or.line(:6)=='DADDR '.or.&
                & line(:6)=='DREAL '.or.line(:7)=='DLREAL ') then
                
            else if (getop(line,0,.false.)/='') then
                dwlabel = .false.
            end if
        end if
        if (line(:3)=='DW '.or.line(:3)=='D8 '.or.line(:4)=='D16 '.or.line(:4)=='D32 '.or.line(:6)=='DADDR '.or.&
        & line(:6)=='DREAL '.or.line(:7)=='DLREAL ') then
            if (dwmemsze=='') then
                call throw('memsize must be declared before dw statments are used')
                
            end if
            if (dwlabel) then
                if (.not.allocated(dws)) then
                    allocate(dws(1))
                    dws(1) = dw(prevLine,count)
                else
                    dws = [dws, dw(prevLine,count)]
                end if
            end if
            if (line(2:2)=='W') then
                sizedw = 1
                type = memszet
            else
                temp = line(2:index(line,' ')-1)
                type = strtype(temp)
                sizedw = ceiling(typesize(type)/real(typesize(memszet)))
            end if
            i = 1
          5 if (line(i:i)=='['.or.line(i:i)==']') line = line(:i-1)//line(i+1:)
            i = i + 1
            if (i<=len(line)) goto 5
            k = 1
            if (.true.) then
                do while (getop(line,1,.false.,k)/='')
                    temp = getop(line,1,.false.,k,l)
                    k = l
                    if (temp(:1)=='"') then
                        skip = .false.
                        temp = temp(2:len(trim(temp))-1)
                        do i=1,len(temp)
                            if (skip) then
                                skip = .false.
                                cycle
                            else if (temp(i:i)=='"') then
                                exit
                            else if (temp(i:i)=='\') then
                                temp2 = '            '
                                write (temp2, '(I0)') count
                                dwlist = dwlist//'*(('//c_type(type)//'*)(Dws'//itoa(id)//&
                                '+'//trim(temp2)//')) = '''//temp(i:i+1)//''';'//achar(10)
                                skip = .true.
                            else
                                temp2 = '            '
                                write (temp2, '(I0)') count
                                dwlist = dwlist// '*(('//c_type(type)//'*)(Dws'//itoa(id)//&
                                '+'//trim(temp2)//')) = '''//temp(i:i)//''';'//achar(10)
                            end if
                            count = count + sizedw
                        end do
                        count = count - sizedw
                    else
                        temp = parseArg(temp,unused,vars,dws)
                        temp2 = '            '
                        write (temp2, '(I0)') count
                        dwlist = dwlist//('*(('//c_type(type)//'*)(Dws'//itoa(id)//&
                        '+'//trim(temp2)//')) = '//temp(2:)//';'//achar(10))
                    end if
                    count = count + sizedw
                end do
            end if
        end if
        call updatecom(line, comment)
        end do

    end function
end module
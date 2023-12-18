module emit
    use var
    implicit none

   contains
    recursive function parseArg(arg, type, vars) result(parse)
        character(len=:), allocatable :: arg, parse, tmpstr,tmpstr2
        type(variable), allocatable :: vars(:)
        integer :: type
        type(variable) :: tmpvar
        integer temp, i
        arg = trim(arg)
        if (arg(:1)=='@') then
            type = 32
            temp = index(arg(2:),'+')+1
            if (temp==1.or.temp>index(arg(2:),'-')+1.and.index(arg(2:),'-')/=0) temp = index(arg(2:),'-')+1
            if (temp==1.or.temp>index(arg,'*').and.index(arg,'*')/=0) temp = index(arg,'*')
            if (temp==0.or.temp>index(arg,'/').and.index(arg,'/')/=0) temp = index(arg,'/')
            tmpstr = arg(:temp-1)
            tmpstr2 = arg(temp+1:)
            if (temp==0) then
                if (tmpstr2(:1)=='@') then
                    tmpstr2 = parseArg(evalDefine(tmpstr2),type,vars)
                end if
                parse = tmpstr2
            else
                if (arch(:1)=='C') then
                    parse = 'I('//'const_'//tmpstr//arg(temp:temp)//tmpstr2//')'
                else
                    select case (arg(temp:temp))
                    case ('+')
                        temp = evalConst(tmpstr) + evalConst(tmpstr2)
                    case ('-')
                        temp = evalConst(tmpstr) + evalConst(tmpstr2)
                    case ('*')
                        temp = evalConst(tmpstr) * evalConst(tmpstr2)
                    case ('/')
                        temp = evalConst(tmpstr) / evalConst(tmpstr2)
                    end select
                    parse = repeat(' ', 12)
                    if (temp>=0.and.temp<2**16) then
                        if(temp<2**8) then
                            type=8
                        else
                            type=16
                        end if
                    end if
                    write(parse,'(I12)') temp
                    parse = 'I'//trim(adjustl(parse))
                end if
            end if
        else if (arg(:1)=='%') then !port
            type = 4
            parse = arg
        else if (arg(:1)=='~') then !relative
            type = 1
            if (arch(:1)=='C') then
                read (arg(2:), *) temp
                allocate(character(len=12) :: parse)
                write (parse, *) lnum2+temp
                parse = 'I&&line'//trim(adjustl(parse))
            else if (arch=='IRI') then
                read (arg(2:), *) temp
                allocate(character(len=12) :: parse)
                write (parse, *) lnum2+temp
                parse = 'I.line'//trim(adjustl(parse))
            end if
        else if (index(arg,'.')==0.and.(arg(:1)=='-'.or.arg(:1)=='+'.or.(arg(:1)>='0'.and.arg(:1)<='9'))) then !int
            !handle inline math
            temp = index(arg(2:),'+')+1
            if (temp==1.or.temp>index(arg(2:),'-')+1.and.index(arg(2:),'-')/=0) temp = index(arg(2:),'-')+1
            if (temp==1.or.temp>index(arg,'*').and.index(arg,'*')/=0) temp = index(arg,'*')
            if (temp==0.or.temp>index(arg,'/').and.index(arg,'/')/=0) temp = index(arg,'/')
            if (temp/=0) then
                if (arch(:1)=='C') then
                    tmpstr = arg(temp+1:)
                    if (tmpstr(:1)=='@') tmpstr = 'const_'//tmpstr(2:)
                    type = 32
                    parse = 'I('//arg(:temp)//tmpstr//')'
                else if (arch=='IRI') then
                    tmpstr2 = arg(temp+1:)
                    tmpstr = arg(:temp-1)
                    select case (arg(temp:temp))
                    case ('+')
                        temp = evalConst(tmpstr) + evalConst(tmpstr2)
                    case ('-')
                        temp = evalConst(tmpstr) + evalConst(tmpstr2)
                    case ('*')
                        temp = evalConst(tmpstr) * evalConst(tmpstr2)
                    case ('/')
                        temp = evalConst(tmpstr) / evalConst(tmpstr2)
                    end select
                    parse = repeat(' ', 12)
                    write(parse,'(I12)') temp
                    parse = 'I'//trim(adjustl(parse))
                end if
            else
                type = 0
                if (index(arg,'I')/=0) then
                    read(arg(index(arg,'I')+1:),*) type
                    arg = arg(:index(arg,'I')-1)
                end if
                if (arg(:2)/='0X') then
                    read (arg, *) temp
                else
                    read (arg(3:),'(Z8)') temp
                    arg(2:2) = 'x'
                end if
                if (type==0) then
                    type = 8
                    if (temp >= 256 .or. temp < -256) type = 16
                    if (temp >= 65536 .or. temp < -65536) type = 32
                end if
                parse = 'I'//arg
            end if
        else if (arg(:1)=='''') then !char
            type = 8
            parse = 'I'//arg
        else if (arg(:1)=='.') then !label
            type = 1
            if (arch(:1)=='C') then
                do i=1,size(dws)
                    if (dws(i)%label==arg) then !dw label
                        allocate(character(len=12) :: parse)
                        write (parse,*) dws(i)%address
                        parse = 'I(Dws+'//trim(adjustl(parse))//')'
                        return
                    end if
                end do
                parse = 'I&&'//'urcl'//arg(2:)
            else
                parse = 'I.label_'//arg(2:)
            end if
        else if (arg(:1)=='-'.or.arg(:1)=='+'.or.(arg(:1)>='0'.and.arg(:1)<='9')) then !real
            if (arg(len(trim(arg)):len(trim(arg)))=='d') then !long real
                type = 3
                if (arch(:1)=='C') parse = 'I'//arg(:len(trim(arg))-1)
            else
                type = 2
                parse = 'I'//arg
            end if
        else if (arg(:1)=='M'.and.arg(2:2)>='0'.and.arg(2:2)<='9') then !memory address
            type = 1
            if (arch(:1)=='C') then
                parse = 'I(mem+'//arg(2:)//')'
            else
                parse = 'I'//arg
            end if
        else !variable
            if (getvar_index(vars, arg)/=0) then
                tmpvar = getvar(vars,arg)
                type = tmpvar%type
                parse = 'V'//arg
            else
                print '(I0,A)', lnum, ': unknown operand type '''//arg//''' (likely undeclared variable)'
                stop -1, quiet=.true.
            end if
        end if
    end function

    subroutine init()
        if (arch(:1)=='C') then
            write(2,'(A)') '#include <stdio.h>',&
            &'#include <stdlib.h>',&
            &'#include <math.h>',&
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
            &'void* vADDR;',&
            &'float vREAL;',&
            &'double vLREAL;',&
            &'};',&
            &'unsigned char* data;',&
            &'int width, height, status;',&
            &'void* mtxptr;',&
            &'void* windowptr;'
            if (incopengl) write(2,'(A)') '#include "include/c11threads.h"'
            write(2,'(A)') 'int run() {',&
            &'union tmp tmp1, tmp2, tmp3;',&
            &'HEADER;'
        else if (arch=='IRI') then
            write(2,'(A)') 'BITS 16','MINREG 25'
        else
            print '(A)', 'init not implemented for this architecture'
        end if
    end subroutine

    subroutine end()
        if (arch(:1)=='C') then
            write(2,'(A)') '}'
            if (incopengl) then
                write(2,'(A)') opengl
            else
                write(2,'(A)') 'int main() { run(); }'
            end if
        else if (arch=='IRI') then
            write(2,'(A)') 'HLT'
            if (incprint32) then
                write(2,'(A)') print32
            end if
            if (incdiv32) then
                write(2,'(A)') div32
            end if
        else
            print '(A)', 'end not implemented for this architecture'
        end if
    end subroutine

    subroutine label(arg1)
        character(len=:), allocatable :: arg1
        if (arch(:1)=='C') then
            call app('urcl'//arg1(2:)//': ;')
        else if (arch=='IRI') then
            call app('.label_'//arg1(2:))
        end if
    end subroutine

    subroutine minmem(arg1)
        character(len=:), allocatable :: arg1
        integer :: temp
        if (memdec) then
            print'(I0,A)', lnum, ': attempt to redefine size of memory'
            stop -1, quiet=.true.
        end if
        memdec = .true.
        if (arch(:1)=='C') then
            call app(c_type(memsze)//' mem['//arg1//'];')
        else if (arch=='IRI') then
            if (memsze==32) then
                read (arg1, *) temp
                call app('MINHEAP '//itoa(temp*2))
            else
                call app('MINHEAP '//arg1)
            end if
        end if
    end subroutine

    subroutine minstack(arg1)
        character(len=:), allocatable :: arg1
        if (stackdec) then
            print'(I0,A)', lnum, ': attempt to redefine size of stack'
            stop -1, quiet=.true.
        end if
        stackdec = .true.
        if (arch(:1)=='C') then
            call app(c_type(memsze)//' stack['//arg1//'];')
            call app('void* sp = stack;')
        end if
    end subroutine

    subroutine mincstack(arg1)
        character(len=:), allocatable :: arg1
        if (cstackdec) then
            print'(I0,A)', lnum, ': attempt to redefine size of call stack'
            stop -1, quiet=.true.
        end if
        cstackdec = .true.
        if (arch(:1)=='C') then
            call app('void* cstack['//arg1//'];')
            call app('int csp = 0;')
        end if
    end subroutine

    subroutine define(name, value, vars)
        character(len=:), allocatable :: name
        character(len=:), allocatable :: value
        character(len=:), allocatable :: result
        type(variable), allocatable :: vars(:)
        type(defined) :: tdefine
        integer :: type
        result = parseArg(value, type, vars)
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
        integer type
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
            print '(A)', 'internal error: typesize not implemented for this architecture'
            stop -1, quiet=.true.
        end if
    end function

    subroutine irisimm(addr,result2,type1,type2)
        integer :: addr,type1,type2,int
        character(len=:), allocatable :: result2
        if (type1/=32) then
            if (type2==32) then
                print'(I0,A)', lnum, ': arg1 of builtin instruction must be of size 32 if arg2 is size 32 for arch IRIS'
                stop -1, quiet=.true.
            end if
            if (type1==8.and.type2/=8) then
                print'(I0,A)', lnum, ': warning: destination of imm is smaller than immediate size'
            end if
            if (addr<=18) then
                if (addr<0) addr=-addr
                if (type1==8.and.type2/=8) then
                    call app('IMM R'//itoa(addr)//' '//result2//' 0xFF')
                else
                    call app('IMM R'//itoa(addr)//' '//result2)
                end if
            else
                if (type1==8.and.type2/=8) then
                    call app('IMM R21 '//result2//' 0xFF')
                    call app('STR M'//itoa(addr-19)//' R21')
                else
                    call app('STR M'//itoa(addr-19)//' '//result2)
                end if
            end if
        else
            read(result2,*) int
            if (addr>0) then
                if (addr<=19) then
                    call app('IMM R'//itoa(addr)//' '//itoa(iand(int,2**16-1)))
                else
                    call app('STR M'//itoa(addr-20)//' '//itoa(iand(int,2**16-1)))
                end if
                if (addr<=18) then
                    call app('IMM R'//itoa(addr+1)//' '//itoa(shiftr(int,16)))
                else
                    call app('STR M'//itoa(addr-19)//' '//itoa(shiftr(int,16)))
                end if
            else
                print*,int
                call app('IMM R'//itoa(-addr)//' '//itoa(iand(int,2**16-1)))
                call app('IMM R'//itoa(-addr+1)//' '//itoa(shiftr(int,16)))
            end if
        end if
    end subroutine

    subroutine imm(arg1,arg2,vars)
        type(variable), allocatable :: vars(:)
        character(len=:), allocatable :: arg1, arg2
        character(len=:), allocatable :: result1, result2
        integer type1, type2, addr
        result1 = parseArg(arg1, type1, vars)
        result2 = parseArg(arg2, type2, vars) !only 2nd arg type is important
        if (result1(:1)/='V') then
            print'(I0, A)', lnum, ': arg1 of IMM must be a variable'
            stop -1, quiet=.true.
        else if (result2(:1)/='I') then
            print'(I0, A)', lnum, ': arg2 of IMM must be an immediate value'
            stop -1, quiet=.true.
        end if
        result1 = result1(2:)
        result2 = result2(2:)
        if (arch(:1)=='C') then
            if (type2==8.or.type2==16) type2 = 32
            call app('tmp1.v'//trim(typestr(type2))//'='//result2//';')
            result2 = 'tmp1.v'//trim(typestr(type1))
            call vars(getvar_index(vars, result1))%set(result2)
        else if (arch=='IRI') then
            addr = vars(getvar_index(vars, result1))%location
            call irisimm(addr,result2,type1,type2)
        end if
    end subroutine

    subroutine parseSmall(result,input,arg,vars)
        type(variable), allocatable :: vars(:)
        character(len=:), allocatable, intent(out) :: result
        character(len=:), allocatable, intent(inout) :: input
        integer, intent(in) :: arg
        integer :: index
        if (input(:1)=='V') then
            input = input(2:)
            index = vars(getvar_index(vars,input))%get(arg)
            result = repeat(' ', 11) ! This line can be removed in fortran 2023
            write(result,'(I0)') index
            result = 'R'//trim(adjustl(result))
        else
            result = input(2:)
        end if
    end subroutine

    subroutine parseBig(result,resultu,input,arg,vars,type)
        type(variable), allocatable :: vars(:)
        character(len=:), allocatable, intent(out) :: result, resultu
        character(len=:), allocatable, intent(inout) :: input
        integer, intent(in) :: arg, type
        integer :: index, temp
        if (input(:1)=='V') then
            input = input(2:)
            index = vars(getvar_index(vars,input))%get(arg)
            result = repeat(' ', 11)
            write(result,'(I0)') index
            result = 'R'//trim(adjustl(result))
            if (type==32) then
                resultu = repeat(' ', 11)
                write(resultu,'(I0)') index + 1
                resultu = 'R'//trim(adjustl(resultu))
            else
                resultu = ''
            end if
        else
            input = input(2:)
            if (input(:2)/='0x') then
                read(input,*) temp
            else
                read(input(3:),'(Z8)') temp
            end if
            if (type==32.or.temp<0) then
                result = repeat(' ',11)
                write(result,'(I0)') iand(temp,2**16-1)
                resultu = repeat(' ',11)
                write(resultu,'(I0)') shiftr(temp,16)
                result = trim(adjustl(result))
                resultu = trim(adjustl(resultu))
            else
                result = input
                resultu = ''
            end if
        end if
    end subroutine

    subroutine standard3Op(inst, arg1, arg2, arg3, vars)
        type(variable), allocatable :: vars(:)
        character(len=:), allocatable :: inst, arg1, arg2, arg3
        character(len=:), allocatable :: result1, result2, result3, stype2, stype3
        character(len=:), allocatable :: output1, output2, output3
        character(len=:), allocatable :: output1u, output2u, output3u
        integer type1, type2, type3, itemp, itemp2, itemp3
        result1 = parseArg(arg1, type1, vars)
        result2 = parseArg(arg2, type2, vars)
        result3 = parseArg(arg3, type3, vars)
        select case (inst)
        case ('LLOD')
            if (type2/=1) then
                print'(I0,A)', lnum, ': arg2 of LLOD must be of type ADDR'
                stop -1, quiet=.true.
            end if
        case ('BGE','BRG','BLE','BRL','BRE','BNE','BRC','BNC','SBGE','SBRG','SBLE','SBRL','SBRC','SBNC','FBGE',&
             &'FBRG','FBLE','FBRL','FBRE','FBNE','LFBGE','LFBRG','LFBLE','LFBRL','LFBRE','LFBNE')
            if (type1/=1) then
                print'(I0,A)', lnum, ': arg1 of '//inst//' must be of type ADDR'
                stop -1, quiet=.true.
            end if
        case default
            if (result1(:1)/='V') then
                print'(I0,A)', lnum, ': arg1 of '//inst//' must be a variable'
                stop -1, quiet=.true.
            end if
        end select
        if (arch=='IRI') then
            select case (inst)
            case ('BGE','BLE','BRE','BRG','BRL','BRC','BNC','SBRG','SBRL')
                if (type2/=32.and.type3/=32) then
                    call parseSmall(output1,result1,1,vars)
                    call parseSmall(output2,result2,2,vars)
                    call parseSmall(output3,result3,3,vars)
                    call app(inst//' '//output1//' '//output2//' '//output3)
                else
                    call parseSmall(output1,result1,1,vars)
                    call parseBig(output2,output2u,result2,2,vars,type2)
                    call parseBig(output3,output3u,result3,3,vars,type3)
                    select case (inst)
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
                    case ('BRE')
                        if (output2u/=''.and.output3u/='') then
                            call app('BNE ~+2 '//output2u//' '//output3u)
                        else if (output3u=='') then
                            call app('BNE ~+2 '//output2u//' R0')
                        else if (output2u=='') then
                            call app('BNE ~+2 '//output3u//' R0')
                        end if
                        call app('BRE '//output1//' '//output2//' '//output3)
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
                    end select
                end if
            case ('ADD','SUB','MLT','DIV','MOD','BSL','BSR','SBSR','NOR','AND','XOR')
                if (type1/=32) then
                    call parseSmall(output2,result2,2,vars)
                    call parseSmall(output3,result3,3,vars)
                    result1 = result1(2:)
                    select case (inst)
                    case ('SBSR')
                        call vars(getvar_index(vars,result1))%set('BSS '//output2//' '//output3)
                    case default
                        call vars(getvar_index(vars,result1))%set(inst//' '//output2//' '//output3)
                    end select
                    if (type1==8) then
                        call parseSmall(output1,result1,1,vars)
                        call vars(getvar_index(vars,result1))%set('AND '//output1//' 0xFF')
                    end if
                else
                    call parseBig(output2,output2u,result2,2,vars,type2)
                    call parseBig(output3,output3u,result3,3,vars,type3)
                    result1 = result1(2:)

                    select case (inst)
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
                        if (itemp==itemp2.or.itemp==itemp3.or.itemp>=18) then
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
                        if (inst=='DIV'.and.itemp/=3) then
                            call vars(getvar_index(vars,result1))%set(' R3')
                            call vars(getvar_index(vars,result1))%set(' R4',.true.)
                        else if (itemp/=5) then
                            call vars(getvar_index(vars,result1))%set(' R5')
                            call vars(getvar_index(vars,result1))%set(' R6',.true.)
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
                                if (inst=='BSR') then
                                    call vars(getvar_index(vars,result1))%set('BSR '//output2u//' '//output3,.true.)
                                else
                                    call vars(getvar_index(vars,result1))%set('BSS '//output2u//' '//output3,.true.)
                                end if
                            else if (itemp==16) then
                                call vars(getvar_index(vars,result1))%set(' '//output2u)
                                if (inst=='BSR') then
                                    call vars(getvar_index(vars,result1))%set(' R0',.true.)
                                else
                                    call parseBig(output1,output1u,result1,1,vars,type1)
                                    call vars(getvar_index(vars,result1))%set('SSETL '//output1u//' R0',.true.)
                                end if
                            else
                                write(output3,'(I0)') itemp-16
                                if (inst=='BSR') then
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
                            call vars(getvar_index(vars,result1))%set('NOR '//output2u//' '//output3u,.true.)
                        else
                            call vars(getvar_index(vars,result1))%set(' R0',.true.)
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
                    end select
                end if
            case ('FADD','FSUB','FMLT','FDIV')
                call parseSmall(output2,result2,2,vars)
                call parseSmall(output3,result3,3,vars)
                result1 = result1(2:)
                call vars(getvar_index(vars,result1))%set(inst//' '//output2//' '//output3)
            case default
                print'(I0,A)', lnum, ': unknown instruction '//inst//' for arch IRIS'
                stop -1, quiet=.true.
            end select
        else if (arch(:1)=='C') then
            result1 = result1(2:)
            result2 = result2(2:)
            result3 = result3(2:)
            stype2 = signed_c_type(type2)
            stype3 = signed_c_type(type3)

            if (type2 >= 8 .and. type2 <= 32) type2 = 32
            if (type3 >= 8 .and. type3 <= 32) type3 = 32
            select case (inst)
            case ('SDIV','SBSR','SBGE','SBRG','SBLE','SBRL','SBRC','SBNC')
                if (type2==32) then
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
            select case (inst)
            case ('ADD')
                call app('tmp1.v32=tmp2.v32+tmp3.v32;')
            case ('SUB')
                call app('tmp1.v32=tmp2.v32-tmp3.v32;')
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
                    print'(I0,A)', lnum, ': size of memory must be declared before use of LLOD'
                    stop -1, quiet=.true.
                end if
                call app('tmp1.v32=*('//c_type(type1)//'*)((long long)tmp2.vADDR+tmp3.v32)')
            end select
            select case (inst)
            case ('BGE','BRG','BLE','BRL','BRE','BNE','BRC','BNC','SBGE','SBRG','SBLE','SBRL','SBRC','SBNC','FBGE','FBRG','FBLE',&
                 &'FBRL','FBRE','FBNE','LFBGE','LFBRG','LFBLE','LFBRL','LFBRE','LFBNE')
            case default
                result2 = 'tmp1.v'//trim(typestr(type1))
                call vars(getvar_index(vars, result1))%set(result2)
            end select
        end if
    end subroutine

    subroutine standard2Op(inst, arg1,arg2,vars)
        type(variable), allocatable :: vars(:)
        character(len=:), allocatable :: arg1, arg2, inst
        character(len=:), allocatable :: result1, result2, stype2
        character(len=:), allocatable :: output1, output2, output2u
        integer type1, type2
        result1 = parseArg(arg1, type1, vars)
        result2 = parseArg(arg2, type2, vars)
        select case (inst)
        case ('BRP','BRN','BRZ','BNZ','FBRP','FBRN','FBRZ','FBNZ','LFBRP','LFBRN','LFBRZ','LFBNZ')
            if (type1/=1) then
                print'(I0,A)', lnum, ': arg1 of '//inst//' must be of type ADDR'
                stop -1, quiet=.true.
            end if
        case default
            if (result1(:1)/='V') then
                print'(I0,A)', lnum, ': arg1 of '//inst//' must be a variable'
                stop -1, quiet=.true.
            end if
        end select
        if (arch=='IRI') then
            select case (inst)
            case ('MOV','INC','LSH','NEG','NOT','ITOF','FTOI')
                result1 = result1(2:)
                if (type1/=32) then
                    call parseSmall(output2,result2,3,vars)
                    call vars(getvar_index(vars, result1))%set(inst//' '//output2)
                else
                    call parseBig(output2,output2u,result2,3,vars,type2)
                    select case (inst)
                    case ('MOV')
                        call vars(getvar_index(vars, result1))%set(' '//output2)
                        call vars(getvar_index(vars, result1))%set(' '//output2u,.true.)
                    case ('INC')
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
                        call vars(getvar_index(vars, result1))%set(inst//' '//output2)
                    case ('FTOI')
                        call vars(getvar_index(vars, result1))%set(inst//' '//output2)
                        call vars(getvar_index(vars, result1))%set(' R0',.true.)
                    end select
                end if
            case ('BRP','BRZ','BNZ')
                call parseSmall(output1,result1,2,vars)
                if (type2/=32) then
                    call parseSmall(output2,result2,3,vars)
                    call app(inst//' '//output1//' '//output2)
                else
                    call parseBig(output2,output2u,result2,3,vars,type2)
                    select case (inst)
                    case ('BRP')
                        call app('BRP '//output1//' '//output2u)
                    case ('BNZ')
                        call app('BNZ '//output1//' '//output2)
                        call app('BNZ '//output1//' '//output2u)
                    case ('BRZ')
                        call app('BNZ ~+2 '//output2u)
                        call app('BRZ '//output1//' '//output2)
                    end select
                end if
            case default
                print'(I0,A)', lnum, ': unknown instruction '//inst//' for arch IRIS'
                stop -1, quiet=.true.
            end select
        else if (arch(:1)=='C') then
            result1 = result1(2:)
            result2 = result2(2:)
            stype2 = signed_c_type(type2)
            if (type2 >= 8 .and. type2 <= 32) type2 = 32
            select case(inst)
            case ('SMOV','SRSH','BRP','BRN')
                if (type2 == 32) call app('tmp2.v'//trim(typestr(type2))//'=('//stype2//')'//result2//';')
                if (type2 /= 32) call app('tmp2.v'//trim(typestr(type2))//'='//result2//';')
            case default
                call app('tmp2.v'//trim(typestr(type2))//'='//result2//';')
            end select
            select case(inst)
            case ('MOV','SMOV')
                call app('tmp1.v32=tmp2.v32;')
            case ('ABS')
                call app('tmp1.v32=abs(tmp2.v32);')
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
            select case (inst)
            case ('BRP','BRN','BRZ','BNZ','FBRP','FBRN','FBRZ','FBNZ','LFBRP','LFBRN','LFBRZ','LFBNZ')
            case default
                call vars(getvar_index(vars, result1))%set(result2)
            end select
        end if
    end subroutine

    subroutine standard1Op(inst,arg1,vars)
        type(variable), allocatable :: vars(:)
        character(len=:), allocatable :: arg1, inst
        character(len=:), allocatable :: result1, output1
        integer type1
        integer tmp
        result1 = parseArg(arg1, type1, vars)
        select case (inst)
        case ('CAL')
            if (type1/=1) then
                print'(I0,A)', lnum, ': arg1 of '//inst//' must be of type ADDR'
                stop -1, quiet=.true.
            end if
            if (.not.cstackdec) then
                print'(I0,A)', lnum, ': size of call stack must be declared before use of CAL'
                stop -1, quiet=.true.
            end if
        case ('JMP')
            if (type1/=1) then
                print'(I0,A)', lnum, ': arg1 of '//inst//' must be of type ADDR'
                stop -1, quiet=.true.
            end if
        case ('PSH')
            if (.not.stackdec) then
                print'(I0,A)', lnum, ': size of stack must be declared before use of PSH'
                stop -1, quiet=.true.
            end if
        case ('POP')
            if (result1(:1)/='V') then
                print'(I0,A)', lnum, ': arg1 of POP must be a variable'
                stop -1, quiet=.true.
            end if
            if (.not.stackdec) then
                print'(I0,A)', lnum, ': size of stack must be declared before use of POP'
                stop -1, quiet=.true.
            end if
        end select
        if (arch(:1)=='C') then
            result1 = result1(2:)
            select case (inst)
            case ('JMP')
                call app('goto *('//result1//');')
            case ('CAL')
                call app('cstack[csp]=&&line'//itoa(lnum2+1)//';')
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
        else if (arch=='IRI') then
            select case (inst)
            case ('CAL')
                call parseSmall(output1,result1,2,vars)
                call app('HCAL '//output1)
            case ('JMP')
                call parseSmall(output1,result1,2,vars)
                call app('JMP '//output1)
            case default
                print'(I0,A)', lnum, ': unknown instruction '//inst//' for arch IRIS'
                stop -1, quiet=.true.
            end select
        end if
    end subroutine

    subroutine str(arg1,arg2,vars)
        type(variable), allocatable :: vars(:)
        character(len=:), allocatable :: arg1, arg2
        character(len=:), allocatable :: result1, result2
        integer type1, type2
        if (.not.memdec) then
            print'(I0,A)', lnum, ': size of memory must be declared before use of STR'
            stop -1, quiet=.true.
        end if
        result1 = parseArg(arg1, type1, vars)
        result2 = parseArg(arg2, type2, vars)
        if (type1/=1) then
            print'(I0,A)', lnum, ': arg1 of STR must be of size ADDR'
            stop -1, quiet=.true.
        end if
        if (arch(:1)=='C') then
            result1 = result1(2:)
            result2 = result2(2:)
            call app('*('//c_type(type2)//'*)'//result1//'='//result2//';')
        end if
    end subroutine

    subroutine lstr(arg1,arg2,arg3,vars)
        type(variable), allocatable :: vars(:)
        character(len=:), allocatable :: arg1, arg2, arg3
        character(len=:), allocatable :: result1, result2, result3
        integer type1, type2, type3
        if (.not.memdec) then
            print'(I0,A)', lnum, ': size of memory must be declared before use of LSTR'
            stop -1, quiet=.true.
        end if
        result1 = parseArg(arg1, type1, vars)
        result2 = parseArg(arg2, type2, vars)
        result3 = parseArg(arg3, type3, vars)
        if (type1/=1) then
            print'(I0,A)', lnum, ': arg1 of LSTR must be of type ADDR'
            stop -1, quiet=.true.
        end if
        if (arch(:1)=='C') then
            result1 = result1(2:)
            result2 = result2(2:)
            result3 = result3(2:)
            if (type1 >= 8 .and. type1 <= 32) type1 = 32
            if (type2 >= 8 .and. type2 <= 32) type2 = 32
            if (type3 >= 8 .and. type3 <= 32) type3 = 32
            call app('tmp1.v'//trim(typestr(type2))//'='//result2//';')
            call app('tmp2.v'//trim(typestr(type2))//'='//result2//';')
            call app('tmp3.v'//trim(typestr(type3))//'='//result3//';')
            call app('*('//c_type(type3)//'*)((int)tmp1.vADDR+tmp2.v32)=tmp3.v32;')
        end if
    end subroutine

    subroutine cpy(arg1,arg2,arg3,vars)
        type(variable), allocatable :: vars(:)
        character(len=:), allocatable :: arg1, arg2, arg3
        character(len=:), allocatable :: result1, result2, result3
        integer type1, type2, type3
        if (.not.memdec) then
            print'(I0,A)', lnum, ': size of memory must be declared before use of CPY'
            stop -1, quiet=.true.
        end if
        result1 = parseArg(arg1, type1, vars)
        result2 = parseArg(arg2, type2, vars)
        result3 = parseArg(arg3, type3, vars)
        if (type1/=1) then
            print'(I0,A)', lnum, ': arg1 of CPY must be of type ADDR'
            stop -1, quiet=.true.
        end if
        if (type2/=1) then
            print'(I0,A)', lnum, ': arg2 of CPY must be of type ADDR'
            stop -1, quiet=.true.
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

    subroutine lod(arg1,arg2,vars)
        type(variable), allocatable :: vars(:)
        character(len=:), allocatable :: arg1, arg2
        character(len=:), allocatable :: result1, result2, output2
        integer type1, type2
        if (.not.memdec) then
            print'(I0,A)', lnum, ': size of memory must be declared before use of LOD'
            stop -1, quiet=.true.
        end if
        result1 = parseArg(arg1, type1, vars)
        result2 = parseArg(arg2, type2, vars)
        if (type2/=1) then
            print'(I0,A)', lnum, ': arg2 of LOD must be of size ADDR'
            stop -1, quiet=.true.
        end if

        result1 = result1(2:)
        if (arch(:1)=='C') then
            result2 = result2(2:)
            call app(result1//'=*('//c_type(type1)//'*)'//result2//';')
        else if (arch=='IRI') then
            call parseSmall(output2,result2,2,vars)
            call vars(getvar_index(vars, result1))%set('LOD '//output2)
            if (type1==32) call vars(getvar_index(vars, result1))%set('LLOD 1 '//output2,.true.)
        end if
    end subroutine

    subroutine ret()
        if (.not.cstackdec) then
            print'(I0,A)', lnum, ': size of call stack must be declared before use of RET'
            stop -1, quiet=.true.
        end if
        if (arch(:1)=='C') then
            call app('csp--;')
            call app('goto *cstack[csp];')
        else if (arch=='IRI') then
            call app('HRET')
        end if
    end subroutine

    subroutine out(arg1,arg2,arg3,arg4,vars)
        type(variable), allocatable :: vars(:)
        character(len=:), allocatable :: arg1, arg2, arg3, arg4
        character(len=:), allocatable :: result1, result2, output2, output2u
        character(len=:), allocatable :: result3, result4, output3, output4
        integer type1, type2
        result1 = parseArg(arg1, type1, vars)
        result2 = parseArg(arg2, type2, vars)
        if (type1/=4) then
            print'(I0,A)', lnum, ': arg1 of OUT must be a port'
            stop -1, quiet=.true.
        end if
        if (arch(:1)=='C') then
            result2 = result2(2:)
            select case (result1)
            case ('%TEXT','%ASCII')
                call app('printf("%c",'//result2//');')
            case ('%NUMB')
                call app('printf("%d",'//result2//');')
            case ('%PIXEL')
                result3 = parseArg(arg3, type1, vars)
                result4 = parseArg(arg4, type1, vars)
                result3 = result3(2:)
                result4 = result4(2:)
                call app('mtx_lock(mtxptr);')
                call app('data['//result3//'*width*4+'//result2//'*4] = '//result4//';')
                call app('data['//result3//'*width*4+'//result2//'*4+1] = '//result4//';')
                call app('data['//result3//'*width*4+'//result2//'*4+2] = '//result4//';')
                call app('mtx_unlock(mtxptr);')
                incopengl = .true.
            case default
                print'(I0,A)', lnum, ': unknown port "'//result2//'" for target C'
            end select
        else if (arch=='IRI') then
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
                result3 = parseArg(arg3, type1, vars)
                result4 = parseArg(arg4, type1, vars)
                call parseSmall(output2,result2,1,vars)
                call parseSmall(output3,result3,2,vars)
                call parseSmall(output4,result4,3,vars)
                call app('OUT %X '//output2//achar(10)//'OUT %Y '//output3)
                call app('OUT %COLOR '//output4)
            end select
        end if

    end subroutine

    subroutine in(arg1,arg2,vars)
        type(variable), allocatable :: vars(:)
        character(len=:), allocatable :: arg1, arg2
        character(len=:), allocatable :: result1, result2
        integer type1, type2
        result1 = parseArg(arg1, type1, vars)
        result2 = parseArg(arg2, type2, vars)
        if (type2/=4) then
            print'(I0,A)', lnum, ': arg2 of IN must be a port'
            stop -1, quiet=.true.
        end if
        if (arch(:1)=='C') then
            result1 = result1(2:)
            select case (result2)
            case ('%TEXT','%ASCII')
                call app('tmp1.v32=getch();')
                result2 = 'tmp1.v'//trim(typestr(type1))
                call vars(getvar_index(vars, result1))%set(result2)
            case ('%SIZEX')
                call app('tmp1.v32=width;')
                result2 = 'tmp1.v'//trim(typestr(type1))
                call vars(getvar_index(vars, result1))%set(result2)
            case ('%SIZEY')
                call app('tmp1.v32=height;')
                result2 = 'tmp1.v'//trim(typestr(type1))
                call vars(getvar_index(vars, result1))%set(result2)
            case ('%EXIT')

                call app('mtx_lock(mtxptr);'//achar(10)//'tmp1.v32=status;'//achar(10)//'mtx_unlock(mtxptr);')
                result2 = 'tmp1.v'//trim(typestr(type1))
                call vars(getvar_index(vars, result1))%set(result2)
            case default
                print'(I0,A)', lnum, ': unknown port "'//result2//'" for target C'
            end select
        else if (arch=='IRI') then
            result1 = result1(2:)
            select case (result2)
            case ('%TEXT')
                call vars(getvar_index(vars, result1))%set('IN %TEXT')
            case ('%SIZEX')
                call vars(getvar_index(vars, result1))%set('IN %X')
            case ('%SIZEY')
                call vars(getvar_index(vars, result1))%set('IN %Y')
            case ('%EXIT')
                call vars(getvar_index(vars, result1))%set(' R0')
            case default
                print'(I0,A)', lnum, ': unknown port "'//result2//'" for target C'
            end select
        end if
    end subroutine

    subroutine data(inst,line,vars)
        type(variable), allocatable :: vars(:)
        character(len=:), allocatable :: line, inst, result1, arg
        integer typedw, type1, temp, i, j
        logical skip
        inst = inst(2:)
        if (inst=='W') then
            typedw = memsze
        else
            typedw = strtype(inst)
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
                        call app('DW '''//arg(j:j)//'''')
                    end if
                    if (typedw==32) call app('DW 0')
                end do
            else
                result1 = parseArg(arg, type1, vars)
                if (typedw/=32) then
                    call app('DW '//result1(2:))
                else
                    read(result1(2:),*) temp
                    call app('DW '//itoa(iand(temp,2**16-1)))
                    call app('DW '//itoa(shiftr(temp,16)))
                end if
            end if
            i = i + 1
            arg = getop(line,i,.false.)
        end do
    end subroutine

    subroutine c_parseDws(count,dwlist,pass)
        implicit none
        logical dwlabel
        integer, intent(out) :: count
        character(len=:), allocatable :: dwmemsze, line, prevLine, temp, temp2, dwlist
        logical comment, skip
        integer i,j,unused
        type(DW) thing
        type(variable), allocatable :: vars(:)
        integer :: size, type, memszet
        character(len=256) :: readline
        integer, intent(in) :: pass
        if (pass==1) then
            allocate(dws(0),vars(0))
        end if
        dwlabel = .false.
        count = 0
        dwmemsze = ''
        comment = .false.
        dwlist = ''
        prevLine = '' !to shut up gfortran
      1 line = ''
      2 read (1, '(A)', advance='no', eor=3, end=4) readline
        line = line//readline
        goto 2
      3 line = line//readline
        call fixstr(line,comment)
        if (line(:7)=='@MEMSZE ') then
            memszet = strtype(getop(line,1))
            dwmemsze = c_type(memszet)
            if (pass==2) call app(dwmemsze//'* Dws;')
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
                print'(I0,A)',lnum,': memsize must be declared before dw statments are used'
                stop -1, quiet=.true.
            end if
            if (dwlabel) then
                thing%address = count
                thing%label = prevLine
                dws = [dws, thing]
            end if
            if (line(2:2)=='W') then
                size = 1
                type = memszet
            else
                temp = line(2:index(line,' ')-1)
                type = strtype(temp)
                size = ceiling(typesize(type)/real(typesize(memszet)))
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
                        else if (temp(i:i)=='\') then
                            temp2 = '            '
                            write (temp2, '(I0)') count
                            dwlist = dwlist//'*('//c_type(type)//'*)(Dws+'//trim(temp2)//') = '''//temp(i:i+1)//''';'//achar(10)
                            skip = .true.
                        else
                            temp2 = '            '
                            write (temp2, '(I0)') count
                            dwlist = dwlist// '*('//c_type(type)//'*)(Dws+'//trim(temp2)//') = '''//temp(i:i)//''';'//achar(10)
                        end if
                        count = count + size
                    end do
                    count = count - size
                else
                    temp = parseArg(temp,unused,vars)
                    temp2 = '            '
                    write (temp2, '(I0)') count
                    dwlist = dwlist//'*('//c_type(type)//'*)(Dws+'//trim(temp2)//') = '//temp(2:)//';'//achar(10)
                end if
                j = j + 1
                count = count + size
            end do
        end if
        call updatecom(line, comment)
        goto 1
      4 continue
    end subroutine
end module
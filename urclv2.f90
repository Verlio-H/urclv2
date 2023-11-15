module compilervars
    ! C  , U16, U32, SLK
    type DW
        character(len=:), allocatable :: label
        integer address
    end type
    character(len=3) :: arch
    integer :: memsze
    integer :: lnum
    integer :: lnum2
    type(DW), allocatable :: dws(:)
    logical :: memdec = .false.
    logical :: stackdec = .false.
    logical :: cstackdec = .false.
end

module var
    use compilervars
    implicit none
    logical, allocatable :: allocated(:)


    type variable
        character(len=:), allocatable :: name
        integer :: location
        integer(kind=1) :: type
     contains
        procedure :: create => var_create
        procedure :: get => var_get
        procedure :: set => var_set
        procedure :: remove => var_remove
        procedure :: print => var_print
    end type
 contains
    integer function getvar_index(vars, name)
        type(variable), allocatable :: vars(:)
        character(len=:), allocatable :: name
        do getvar_index=1,size(vars)
            if (vars(getvar_index)%name == name) return
        end do
        getvar_index = 0
    end function

    function getvar(vars, name)
        type(variable) getvar
        type(variable), allocatable :: vars(:)
        character(len=:), allocatable :: name
        integer temp
        temp = getvar_index(vars, name)
        if (temp/=0) then
            getvar = vars(temp)
        end if
    end function

    subroutine var_print(this)
        class(variable) this
        print'(A)', this%name
        print'(I0)', this%location
        print'(I0)', this%type
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
            print '(A)', 'typesize not implemented for this architecture'
        end if
    end function

    function strtype(input)
        character(len=:), allocatable, intent(in) :: input
        integer strtype
        select case (trim(adjustl(input)))
        case ('32')
            strtype = 32
        case ('16')
            strtype = 16
        case ('8')
            strtype = 8
        case ('ADDR')
            strtype = 1
        case ('REAL')
            strtype = 2
        case ('LREAL')
            strtype = 3
        case default
            print '(I0, A)', lnum, ': unknown data type "'//input//'"'
            stop
        end select
    end function

    function typestr(input)
        integer, intent(in) :: input
        character(len=5) :: typestr
        select case (input)
        case (32)
            typestr = '32'
        case (16)
            typestr = '16'
        case (8)
            typestr = '8'
        case (1)
            typestr = 'ADDR'
        case (2)
            typestr = 'REAL'
        case (3)
            typestr = 'LREAL'
        case default
            print '(I0, A)', lnum, ': internal error, unknown type number ',input
            stop
        end select
    end function

    function c_type(type)
        character(len=:), allocatable :: c_type
        integer, intent(in) :: type
        select case (type)
        case (1)
            c_type = 'void*'
            return
        case (2)
            c_type = 'float'
            return
        case (3)
            c_type = 'double'
            return
        case (8)
            c_type = 'unsigned char'
            return
        case (16)
            c_type = 'unsigned short'
            return
        case (32)
            c_type = 'unsigned int'
        case default
            print '(I0, A)', lnum, ': unsupport data type "', type, '" for c'
            stop
        end select
    end function

    function signed_c_type(type)
        character(len=:), allocatable :: signed_c_type
        integer, intent(in) :: type
        select case (type)
        case (1)
            signed_c_type = 'void*'
            return
        case (2)
            signed_c_type = 'float'
            return
        case (3)
            signed_c_type = 'double'
            return
        case (8)
            signed_c_type = 'char'
            return
        case (16)
            signed_c_type = 'short'
            return
        case (32)
            signed_c_type = 'int'
        case default
            print '(I0, A)', lnum, ': unsupport data type "', type, '" for c'
            stop
        end select
    end function

    subroutine var_create(this, type, name)
        class(variable) this
        integer, intent(in) :: type
        character(len=*), intent(in) :: name
        if (arch(:1) == 'C') then
            write(2, '(A)') c_type(type)//' '//trim(name)//';'
            this%type = int(type, 1)
            this%name = trim(name)
            return
        end if
        print '(A)', 'var creation not implemented for this target'
        stop
    end subroutine

    subroutine var_get(this, dest)
        class(variable) this
        character(len=:), allocatable :: dest
        if (arch(:1) == 'C') then
            write(2, '(A)') dest//'='//this%name//';'
        else
            print '(A)', 'var get not implemented for this target'
        end if
    end subroutine

    subroutine var_set(this, src)
        class(variable) this
        character(len=:), allocatable :: src
        if (arch(:1) == 'C') then
            write(2, '(A)') this%name//'='//src//';'
        else
            print '(A)', 'var set not implemented for this target'
        end if
    end subroutine

    subroutine var_remove(this)
        class(variable) this
        if (arch(:1) == 'C') then
            this%name=this%name !scuffed way to silence warning
            continue
        else
            print '(A)', 'var remove not implemented for this target'
        end if
    end subroutine
end module

module emit
    use var
    implicit none

   contains
    function parseArg(arg, type, vars)
        character(len=:), allocatable :: arg, parseArg, tmpstr,tmpstr2
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
            if (arch(:1)=='C'.and.tmpstr/='') tmpstr = 'const_'//tmpstr(2:)
            if (arch(:1)=='C'.and.tmpstr2(:1)=='@') tmpstr2 = 'const_'//tmpstr2(2:)
            if (temp==0) then
                parseArg = 'I'//tmpstr2
            else
                parseArg = 'I('//tmpstr//arg(temp:temp)//tmpstr2//')'
            end if
        else if (arg(:1)=='%') then !port
            type = 4
            parseArg = arg
        else if (arg(:1)=='~') then !relative
            type = 1
            if (arch(:1)=='C') then
                read (arg(2:), *) temp
                allocate(character(len=12) :: parseArg)
                write (parseArg, *) lnum2+temp
                parseArg = 'I&&line'//trim(adjustl(parseArg))
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
                    parseArg = 'I('//arg(:temp)//tmpstr//')'
                end if
            else
                read (arg, *) temp
                type = 8
                if (temp >= 256) type = 16
                if (temp >= 65536) type = 32
                parseArg = 'I'//arg
            end if
        else if (arg(:1)=='''') then !char
            type = 8
            parseArg = 'I'//arg
        else if (arg(:1)=='.') then !label
            type = 1
            if (arch(:1)=='C') then
                do i=1,size(dws)
                    if (dws(i)%label==arg) then !dw label
                        allocate(character(len=12) :: parseArg)
                        write (parseArg,*) dws(i)%address
                        parseArg = 'I(Dws+'//trim(adjustl(parseArg))//')'
                        return
                    end if
                end do
                parseArg = 'I&&'//'urcl'//arg(2:)
            else
                parseArg = 'I'//arg
            end if
        else if (arg(:1)=='-'.or.arg(:1)=='+'.or.(arg(:1)>='0'.and.arg(:1)<='9')) then !real
            if (arg(len(trim(arg)):len(trim(arg)))=='d') then !long real
                type = 3
                if (arch(:1)=='C') parseArg = 'I'//arg(:len(trim(arg))-1)
            else
                type = 2
                parseArg = 'I'//arg
            end if
        else if (arg(:1)=='M'.and.arg(2:2)>='0'.and.arg(2:2)<='9') then !memory address
            type = 1
            if (arch(:1)=='C') then
                parseArg = 'I(mem+'//arg(2:)//')'
            else
                parseArg = 'I'//arg
            end if
        else !variable
            if (getvar_index(vars, arg)/=0) then
                tmpvar = getvar(vars,arg)
                type = tmpvar%type
                parseArg = 'V'//arg
            else
                print '(I0,A)', lnum, ': unknown operand type '''//arg//''' (likely undeclared variable)'
                stop
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
            &'};'
            write(2,'(A)') 'int main() {',&
            &'union tmp tmp1, tmp2, tmp3;',&
            &'HEADER;'
        else
            print '(A)', 'init not implemented for this architecture'
        end if
    end subroutine

    subroutine end()
        if (arch(:1)=='C') then
            write(2,'(A)') '}'
        else
            print '(A)', 'end not implemented for this architecture'
        end if
    end subroutine

    subroutine label(arg1)
        character(len=:), allocatable :: arg1
        if (arch(:1)=='C') then
            write (2,'(A)') 'urcl'//arg1(2:)//': ;'
        end if
    end subroutine

    subroutine minmem(arg1)
        character(len=:), allocatable :: arg1
        if (memdec) then
            print'(I0,A)', lnum, ': attempt to redefine size of memory'
            stop
        end if
        memdec = .true.
        if (arch(:1)=='C') then
            write (2, '(A)') c_type(memsze)//' mem['//arg1//'];'
        end if
    end subroutine

    subroutine minstack(arg1)
        character(len=:), allocatable :: arg1
        if (stackdec) then
            print'(I0,A)', lnum, ': attempt to redefine size of stack'
            stop
        end if
        stackdec = .true.
        if (arch(:1)=='C') then
            write (2, '(A)') c_type(memsze)//' stack['//arg1//'];'
            write (2, '(A)') 'void* sp = stack;'
        end if
    end subroutine

    subroutine mincstack(arg1)
        character(len=:), allocatable :: arg1
        if (cstackdec) then
            print'(I0,A)', lnum, ': attempt to redefine size of call stack'
            stop
        end if
        cstackdec = .true.
        if (arch(:1)=='C') then
            write (2, '(A)') 'void* cstack['//arg1//'];'
            write (2, '(A)') 'int csp = 0;'
        end if
    end subroutine

    subroutine imm(arg1,arg2,vars)
        type(variable), allocatable :: vars(:)
        character(len=:), allocatable :: arg1, arg2
        character(len=:), allocatable :: result1, result2
        integer type1, type2
        result1 = parseArg(arg1, type1, vars)
        result2 = parseArg(arg2, type2, vars) !only 2nd arg type is important
        if (result1(:1)/='V') then
            print'(I0, A)', lnum, ': arg1 of IMM must be a variable'
            stop
        else if (result2(:1)/='I') then
            print'(I0, A)', lnum, ': arg2 of IMM must be an immediate value'
            stop
        end if
        result1 = result1(2:)
        result2 = result2(2:)
        if (arch(:1)=='C') then
            write (2, '(A)') 'tmp1.v'//trim(typestr(type2))//'='//result2//';'
            result2 = 'tmp1.v'//trim(typestr(type1))
            call vars(getvar_index(vars, result1))%set(result2)
        end if
    end subroutine

    subroutine standard3Op(inst, arg1, arg2, arg3, vars)
        type(variable), allocatable :: vars(:)
        character(len=:), allocatable :: inst, arg1, arg2, arg3
        character(len=:), allocatable :: result1, result2, result3, stype2, stype3
        integer type1, type2, type3
        result1 = parseArg(arg1, type1, vars)
        result2 = parseArg(arg2, type2, vars)
        result3 = parseArg(arg3, type3, vars)
        select case (inst)
        case ('LLOD')
            if (type2/=1) then
                print'(I0,A)', lnum, ': arg2 of LLOD must be of type ADDR'
                stop
            end if
        case ('BGE','BRG','BLE','BRL','BRE','BNE','BRC','BNC','SBGE','SBRG','SBLE','SBRL','SBRC','SBNC','FBGE',&
             &'FBRG','FBLE','FBRL','FBRE','FBNE','LFBGE','LFBRG','LFBLE','LFBRL','LFBRE','LFBNE')
            if (type1/=1) then
                print'(I0,A)', lnum, ': arg1 of '//inst//' must be of type ADDR'
                stop
            end if
        case default
            if (result1(:1)/='V') then
                print'(I0,A)', lnum, ': arg1 of '//inst//' must be a variable'
                stop
            end if
        end select
        if (arch(:1)=='C') then
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
                    write (2, '(A)') 'tmp2.v'//trim(typestr(type2))//'=('//stype2//')'//result2//';'
                    write (2, '(A)') 'tmp3.v'//trim(typestr(type3))//'=('//stype3//')'//result3//';'
                else
                    write (2, '(A)') 'tmp2.v'//trim(typestr(type2))//'='//result2//';'
                    write (2, '(A)') 'tmp3.v'//trim(typestr(type3))//'='//result3//';'
                end if
            case default
                write (2, '(A)') 'tmp2.v'//trim(typestr(type2))//'='//result2//';'
                write (2, '(A)') 'tmp3.v'//trim(typestr(type3))//'='//result3//';'
            end select
            select case (inst)
            case ('ADD')
                write (2, '(A)') 'tmp1.v32=tmp2.v32+tmp3.v32;'
            case ('SUB')
                write (2, '(A)') 'tmp1.v32=tmp2.v32-tmp3.v32;'
            case ('MLT')
                write (2, '(A)') 'tmp1.v32=tmp2.v32*tmp3.v32;'
            case ('DIV')
                write (2, '(A)') 'tmp1.v32=tmp2.v32/tmp3.v32;'
            case ('MOD')
                write (2, '(A)') 'tmp1.v32=tmp2.v32%tmp3.v32;'
            case ('SDIV')
                write (2, '(A)') 'tmp1.v32=(int)tmp2.v32/(int)tmp3.v32;'
            case ('FADD')
                write (2, '(A)') 'tmp1.vREAL=tmp2.vREAL+tmp3.vREAL;'
            case ('FSUB')
                write (2, '(A)') 'tmp1.vREAL=tmp2.vREAL-tmp3.vREAL;'
            case ('FMLT')
                write (2, '(A)') 'tmp1.vREAL=tmp2.vREAL*tmp3.vREAL;'
            case ('FDIV')
                write (2, '(A)') 'tmp1.vREAL=tmp2.vREAL/tmp3.vREAL;'
            case ('FMOD')
                write (2, '(A)') 'tmp1.vREAL=fmod(tmp2.vREAL,tmp3.vREAL);'
            case ('LFADD')
                write (2, '(A)') 'tmp1.vLREAL=tmp2.vLREAL+tmp3.vLREAL;'
            case ('LFSUB')
                write (2, '(A)') 'tmp1.vLREAL=tmp2.vLREAL-tmp3.vLREAL;'
            case ('LFMLT')
                write (2, '(A)') 'tmp1.vLREAL=tmp2.vLREAL*tmp3.vLREAL;'
            case ('LFDIV')
                write (2, '(A)') 'tmp1.vLREAL=tmp2.vLREAL/tmp3.vLREAL;'
            case ('LFMOD')
                write (2, '(A)') 'tmp1.vLREAL=fmod(tmp2.vLREAL,tmp3.vLREAL);'
            case ('BSL')
                write (2, '(A)') 'tmp1.v32=tmp2.v32<<tmp3.v32;'
            case ('BSR')
                write (2, '(A)') 'tmp1.v32=tmp2.v32>>tmp3.v32;'
            case ('SBSR')
                write (2, '(A)') 'tmp1.v32=(int)tmp2.v32>>tmp3.v32;'
            case ('AND')
                write (2, '(A)') 'tmp1.v32=tmp2.v32&tmp3.v32;'
            case ('OR')
                write (2, '(A)') 'tmp1.v32=tmp2.v32|tmp3.v32;'
            case ('XOR')
                write (2, '(A)') 'tmp1.v32=tmp2.v32^tmp3.v32;'
            case ('NAND')
                write (2, '(A)') 'tmp1.v32=~(tmp2.v32&tmp3.v32);'
            case ('NOR')
                write (2, '(A)') 'tmp1.v32=~(tmp2.v32|tmp3.v32);'
            case ('XNOR')
                write (2, '(A)') 'tmp1.v32=~(tmp2.v32^tmp3.v32);'
            case ('BGE')
                write (2, '(A)') 'if (tmp2.v32>=tmp3.v32) goto *('//result1//');'
            case ('BRG')
                write (2, '(A)') 'if (tmp2.v32>tmp3.v32) goto *('//result1//');'
            case ('BLE')
                write (2, '(A)') 'if (tmp2.v32<=tmp3.v32) goto *('//result1//');'
            case ('BRL')
                write (2, '(A)') 'if (tmp2.v32<tmp3.v32) goto *('//result1//');'
            case ('BRE')
                write (2, '(A)') 'if (tmp2.v32==tmp3.v32) goto *('//result1//');'
            case ('BNE')
                write (2, '(A)') 'if (tmp2.v32!=tmp3.v32) goto *('//result1//');'
            case ('BRC')
                write (2, '(A)') 'if ((tmp2.v32+tmp3.v32)<tmp2.v32) goto *('//result1//');'
            case ('BNC')
                write (2, '(A)') 'if (!((tmp2.v32+tmp3.v32)<tmp2.v32)) goto *('//result1//');'
            case ('SBGE')
                write (2, '(A)') 'if ((int)tmp2.v32>=(int)tmp3.v32) goto *('//result1//');'
            case ('SBRG')
                write (2, '(A)') 'if ((int)tmp2.v32>(int)tmp3.v32) goto *('//result1//');'
            case ('SBLE')
                write (2, '(A)') 'if ((int)tmp2.v32<=(int)tmp3.v32) goto *('//result1//');'
            case ('SBRL')
                write (2, '(A)') 'if ((int)tmp2.v32<(int)tmp3.v32) goto *('//result1//');'
            case ('SBRC')
                write (2, '(A)') 'if ((int)tmp2.v32+tmp3.v32<(int)tmp2.v32) goto *('//result1//');'
            case ('SBNC')
                write (2, '(A)') 'if (!((int)tmp2.v32+tmp3.v32<(int)tmp2.v32)) goto *('//result1//');'
            case ('FBGE')
                write (2, '(A)') 'if (tmp2.vREAL>=tmp3.vREAL) goto *('//result1//');'
            case ('FBRG')
                write (2, '(A)') 'if (tmp2.vREAL>tmp3.vREAL) goto *('//result1//');'
            case ('FBLE')
                write (2, '(A)') 'if (tmp2.vREAL<=tmp3.vREAL) goto *('//result1//');'
            case ('FBRL')
                write (2, '(A)') 'if (tmp2.vREAL<tmp3.vREAL) goto *('//result1//');'
            case ('FBRE')
                write (2, '(A)') 'if (tmp2.vREAL==tmp3.vREAL) goto *('//result1//');'
            case ('FBNE')
                write (2, '(A)') 'if (tmp2.vREAL!=tmp3.vREAL) goto *('//result1//');'
            case ('LFBGE')
                write (2, '(A)') 'if (tmp2.vLREAL>=tmp3.vLREAL) goto *('//result1//');'
            case ('LFBRG')
                write (2, '(A)') 'if (tmp2.vLREAL>tmp3.vLREAL) goto *('//result1//');'
            case ('LFBLE')
                write (2, '(A)') 'if (tmp2.vLREAL<=tmp3.vLREAL) goto *('//result1//');'
            case ('LFBRL')
                write (2, '(A)') 'if (tmp2.vLREAL<tmp3.vLREAL) goto *('//result1//');'
            case ('LFBRE')
                write (2, '(A)') 'if (tmp2.vLREAL==tmp3.vLREAL) goto *('//result1//');'
            case ('LFBNE')
                write (2, '(A)') 'if (tmp2.vLREAL!=tmp3.vLREAL) goto *('//result1//');'
            case ('LLOD')
                if (.not.memdec) then
                    print'(I0,A)', lnum, ': size of memory must be declared before use of LLOD'
                    stop
                end if
                write (2, '(A)') 'tmp1.v32=*('//c_type(type1)//'*)((long long)tmp2.vADDR+tmp3.v32)'
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
        integer type1, type2
        result1 = parseArg(arg1, type1, vars)
        result2 = parseArg(arg2, type2, vars)
        select case (inst)
        case ('BRP','BRN','BRZ','BNZ','FBRP','FBRN','FBRZ','FBNZ','LFBRP','LFBRN','LFBRZ','LFBNZ')
            if (type1/=1) then
                print'(I0,A)', lnum, ': arg1 of '//inst//' must be of type ADDR'
                stop
            end if
        case default
            if (result1(:1)/='V') then
                print'(I0,A)', lnum, ': arg1 of '//inst//' must be a variable'
                stop
            end if
        end select
        if (arch(:1)=='C') then
            result1 = result1(2:)
            result2 = result2(2:)
            stype2 = signed_c_type(type2)
            if (type2 >= 8 .and. type2 <= 32) type2 = 32
            select case(inst)
            case ('SMOV','SRSH','BRP','BRN')
                if (type2 == 32) write (2, '(A)') 'tmp2.v'//trim(typestr(type2))//'=('//stype2//')'//result2//';'
                if (type2 /= 32) write (2, '(A)') 'tmp2.v'//trim(typestr(type2))//'='//result2//';'
            case default
                write (2, '(A)') 'tmp2.v'//trim(typestr(type2))//'='//result2//';'
            end select
            select case(inst)
            case ('MOV','SMOV')
                write (2,'(A)') 'tmp1.v32=tmp2.v32;'
            case ('ABS')
                write (2,'(A)') 'tmp1.v32=abs(tmp2.v32);'
            case ('NEG')
                write (2,'(A)') 'tmp1.v32=-tmp2.v32;'
            case ('RSH')
                write (2,'(A)') 'tmp1.v32=tmp2.v32>>1;'
            case ('SRSH')
                write (2,'(A)') 'tmp1.v32=(int)tmp2.v32>>1;'
            case ('INC')
                write (2,'(A)') 'tmp1.v32=tmp2.v32+1;'
            case ('DEC')
                write (2,'(A)') 'tmp1.v32=tmp2.v32-1;'
            case ('NOT')
                write (2,'(A)') 'tmp1.v32=~tmp2.v32;'
            case ('BRP')
                write (2,'(A)') 'if ((int)tmp2.v32>=0) goto *('//result1//');'
            case ('BRN')
                write (2,'(A)') 'if ((int)tmp2.v32<0) goto *('//result1//');'
            case ('BRZ')
                write (2,'(A)') 'if (tmp2.v32==0) goto *('//result1//');'
            case ('BNZ')
                write (2,'(A)') 'if (tmp2.v32!=0) goto *('//result1//');'
            case ('FABS')
                write (2,'(A)') 'tmp1.vREAL=fabs(tmp2.vREAL);'
            case ('FNEG')
                write (2,'(A)') 'tmp1.vREAL=-tmp2.vREAL;'
            case ('FBRP')
                write (2,'(A)') 'if (tmp2.vREAL>=0) goto *('//result1//');'
            case ('FBRN')
                write (2,'(A)') 'if (tmp2.vREAL<0) goto *('//result1//');'
            case ('FBRZ')
                write (2,'(A)') 'if (tmp2.vREAL==0) goto *('//result1//');'
            case ('FBNZ')
                write (2,'(A)') 'if (tmp2.vREAL!=0) goto *('//result1//');'
            case ('LFBRP')
                write (2,'(A)') 'if (tmp2.vLREAL>=0) goto *('//result1//');'
            case ('LFBRN')
                write (2,'(A)') 'if (tmp2.vLREAL<0) goto *('//result1//');'
            case ('LFBRZ')
                write (2,'(A)') 'if (tmp2.vLREAL==0) goto *('//result1//');'
            case ('LFBNZ')
                write (2,'(A)') 'if (tmp2.vLREAL!=0) goto *('//result1//');'
            case ('ITOF')
                write (2,'(A)') 'tmp1.vREAL=(int)tmp2.v32;'
            case ('FTOI')
                write (2,'(A)') 'tmp1.v32=(int)tmp2.vREAL;'
            case ('ITOLF')
                write (2,'(A)') 'tmp1.vLREAL=(int)tmp2.v32;'
            case ('LFTOI')
                write (2,'(A)') 'tmp1.v32=(int)tmp2.vLREAL;'
            case ('FTOLF')
                write (2,'(A)') 'tmp1.vLREAL=tmp2.vREAL;'
            case ('LFTOF')
                write (2,'(A)') 'tmp1.vREAL=(float)tmp2.vLREAL;'
            end select
            result2 = 'tmp1.v'//trim(typestr(type1))
            call vars(getvar_index(vars, result1))%set(result2)
        end if
    end subroutine

    subroutine standard1Op(inst,arg1,vars)
        type(variable), allocatable :: vars(:)
        character(len=:), allocatable :: arg1, inst
        character(len=:), allocatable :: result1
        integer type1
        integer tmp
        result1 = parseArg(arg1, type1, vars)
        select case (inst)
        case ('JMP','CAL')
            if (type1/=1) then
                print'(I0,A)', lnum, ': arg1 of '//inst//' must be of type ADDR'
                stop
            end if
        case ('POP')
            if (result1(:1)/='V') then
                print'(I0,A)', lnum, ': arg1 of POP must be a variable'
                stop
            end if
        end select
        result1 = result1(2:)
        if (arch(:1)=='C') then
            select case (inst)
            case ('JMP')
                write (2,'(A)') 'goto *('//result1//');'
            case ('CAL')
                if (.not.cstackdec) then
                    print'(I0,A)', lnum, ': size of call stack must be declared before use of CAL'
                    stop
                end if
                write (2,'(A,I0,A)') 'cstack[csp]=&&line',lnum2+1,';'
                write (2,'(A)') 'csp++;'
                write (2,'(A)') 'goto *('//result1//');'
            case ('PSH')
                if (.not.stackdec) then
                    print'(I0,A)', lnum, ': size of stack must be declared before use of PSH'
                    stop
                end if
                tmp = memsze
                if (typesize(type1)>typesize(tmp)) tmp = type1
                write (2,'(A)') '*('//c_type(tmp)//'*)sp='//result1//';'
                write (2,'(A)') 'sp+=1+(sizeof('//c_type(tmp)//')-1)/sizeof('//c_type(memsze)//');'
            case ('POP')
                if (.not.cstackdec) then
                    print'(I0,A)', lnum, ': size of stack must be declared before use of POP'
                    stop
                end if
                tmp = memsze
                if (typesize(type1)>typesize(tmp)) tmp = type1
                write (2,'(A)') 'sp-=1+(sizeof('//c_type(tmp)//')-1)/sizeof('//c_type(memsze)//');'
                write (2,'(A)') result1//'=*('//c_type(tmp)//'*)sp;'
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
            stop
        end if
        result1 = parseArg(arg1, type1, vars)
        result2 = parseArg(arg2, type2, vars)
        if (type1/=1) then
            print'(I0,A)', lnum, ': arg1 of STR must be of size ADDR'
            stop
        end if
        if (arch(:1)=='C') then
            result1 = result1(2:)
            result2 = result2(2:)
            write (2,'(A)') '*('//c_type(type2)//'*)'//result1//'='//result2//';'
        end if
    end subroutine

    subroutine lstr(arg1,arg2,arg3,vars)
        type(variable), allocatable :: vars(:)
        character(len=:), allocatable :: arg1, arg2, arg3
        character(len=:), allocatable :: result1, result2, result3
        integer type1, type2, type3
        if (.not.memdec) then
            print'(I0,A)', lnum, ': size of memory must be declared before use of LSTR'
            stop
        end if
        result1 = parseArg(arg1, type1, vars)
        result2 = parseArg(arg2, type2, vars)
        result3 = parseArg(arg3, type3, vars)
        if (type1/=1) then
            print'(I0,A)', lnum, ': arg1 of LSTR must be of type ADDR'
            stop
        end if
        if (arch(:1)=='C') then
            result1 = result1(2:)
            result2 = result2(2:)
            result3 = result3(2:)
            if (type1 >= 8 .and. type1 <= 32) type1 = 32
            if (type2 >= 8 .and. type2 <= 32) type2 = 32
            if (type3 >= 8 .and. type3 <= 32) type3 = 32
            write (2, '(A)') 'tmp1.v'//trim(typestr(type2))//'='//result2//';'
            write (2, '(A)') 'tmp2.v'//trim(typestr(type2))//'='//result2//';'
            write (2, '(A)') 'tmp3.v'//trim(typestr(type3))//'='//result3//';'
            write (2, '(A)') '*('//c_type(type3)//'*)((int)tmp1.vADDR+tmp2.v32)=tmp3.v32;'
        end if
    end subroutine

    subroutine cpy(arg1,arg2,arg3,vars)
        type(variable), allocatable :: vars(:)
        character(len=:), allocatable :: arg1, arg2, arg3
        character(len=:), allocatable :: result1, result2, result3
        integer type1, type2, type3
        if (.not.memdec) then
            print'(I0,A)', lnum, ': size of memory must be declared before use of CPY'
            stop
        end if
        result1 = parseArg(arg1, type1, vars)
        result2 = parseArg(arg2, type2, vars)
        result3 = parseArg(arg3, type3, vars)
        if (type1/=1) then
            print'(I0,A)', lnum, ': arg1 of CPY must be of type ADDR'
            stop
        end if
        if (type2/=1) then
            print'(I0,A)', lnum, ': arg2 of CPY must be of type ADDR'
            stop
        end if
        if (arch(:1)=='C') then
            result1 = result1(2:)
            result2 = result2(2:)
            result3 = result3(2:)
            if (type1 >= 8 .and. type1 <= 32) type2 = 32
            if (type2 >= 8 .and. type2 <= 32) type2 = 32
            if (type3 >= 8 .and. type3 <= 32) type3 = 32
            write (2, '(A)') 'tmp1.v'//trim(typestr(type2))//'='//result2//';'
            write (2, '(A)') 'tmp2.v'//trim(typestr(type2))//'='//result2//';'
            write (2, '(A)') 'tmp3.v'//trim(typestr(type3))//'='//result3//';'
            write (2, '(A)') 'memcpy(tmp1.vADDR, tmp2.vADDR, tmp3.v32);'
        end if
    end subroutine

    subroutine lod(arg1,arg2,vars)
        type(variable), allocatable :: vars(:)
        character(len=:), allocatable :: arg1, arg2
        character(len=:), allocatable :: result1, result2
        integer type1, type2
        if (.not.memdec) then
            print'(I0,A)', lnum, ': size of memory must be declared before use of LOD'
            stop
        end if
        result1 = parseArg(arg1, type1, vars)
        result2 = parseArg(arg2, type2, vars)
        if (type2/=1) then
            print'(I0,A)', lnum, ': arg2 of LOD must be of size ADDR'
            stop
        end if
        if (arch(:1)=='C') then
            result1 = result1(2:)
            result2 = result2(2:)
            write (2,'(A)') result1//'=*('//c_type(type1)//'*)'//result2//';'
        end if
    end subroutine

    subroutine ret()
        if (.not.cstackdec) then
            print'(I0,A)', lnum, ': size of call stack must be declared before use of RET'
            stop
        end if
        if (arch(:1)=='C') then
            write (2,'(A)') 'csp--;'
            write (2,'(A)') 'goto *cstack[csp];'
        end if
    end subroutine

    subroutine out(arg1,arg2,vars)
        type(variable), allocatable :: vars(:)
        character(len=:), allocatable :: arg1, arg2
        character(len=:), allocatable :: result1, result2
        integer type1, type2
        result1 = parseArg(arg1, type1, vars)
        result2 = parseArg(arg2, type2, vars)
        if (type1/=4) then
            print'(I0,A)', lnum, ': arg1 of OUT must be a port'
            stop
        end if
        if (arch(:1)=='C') then
            result2 = result2(2:)
            select case (result1)
            case ('%TEXT','%ASCII')
                write(2,'(A)') 'printf("%c",'//result2//');'
            case ('%NUMB')
                write(2,'(A)') 'printf("%d",'//result2//');'
            case default
                print'(I0,A)', lnum, ': unknown port "'//result2//'" for target C'
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
            stop
        end if
        if (arch(:1)=='C') then
            result1 = result1(2:)
            select case (result2)
            case ('%TEXT','%ASCII')
                write(2,'(A)') 'tmp1.v32=getch();'
                result2 = 'tmp1.v'//trim(typestr(type1))
                call vars(getvar_index(vars, result1))%set(result2)
            case default
                print'(I0,A)', lnum, ': unknown port "'//result2//'" for target C'
            end select
        end if
    end subroutine
end module

module helper
    use emit
    implicit none
   contains

    subroutine fixstr(line, comment)
        implicit none
        character(len=:), allocatable :: line
        integer tmp,i
        logical, intent(inout) :: comment
        line = trim(adjustl(line))
        tmp = 0
        if (comment.and.index(line,'*/')==0) then
            line = ''
            return
        end if
        if (comment) line = line(index(line,'*/')+2:)
        do i=1, len(line)
            if (tmp/=2 .and. line(i:i)==' ') then
                tmp = 0
            else if (tmp==0) then
                if (line(i:i)=='.') then
                    tmp = 1
                else if (line(i:i)=='''' .or. line(i:i)=='"') then
                    tmp = 2
                else if (line(i:i)>='a' .and. line(i:i)<='z') then
                        line(i:i) = achar(iachar(line(i:i)) - 32)
                end if
          else if (tmp==2.and.(line(i:i)==''''.or.line(i:i)=='"')) then
                tmp = 0
           end if
        end do

        i = 1
        tmp = len(line)
        do while ((i+1)<=tmp)
            if (line(i:i+1)=='//') exit
            if (line(i:i+1)=='/*'.and.line(i-1:i-1)/=' ') then
                line = line(:i-1)//' /*'//line(i+2:)
                i = i+2
                tmp = tmp + 1
            end if
            i = i + 1
        end do
        comment = .false.
    end subroutine

    subroutine updatecom(line, comment)
        character(len=:), allocatable :: line
        logical comment, skip
        integer i
        do i=1,len(line)-1
            if (skip) then
                skip=.false.
                cycle
            end if
            if (line(i:i+1)=='//') return
            if (line(i:i+1)=='/*') then
                comment = .true.
                skip = .true.
            else if (line(i:i+1)=='*/') then
                comment = .false.
                skip = .true.
            end if
        end do
    end subroutine

    function getop(line, num, error)
        character(len=:), allocatable :: line, getop, tmp
        integer num, i, j, group, start
        logical str, skip
        logical, optional :: error
        !logical, target, intent(inout) :: com
        !logical, target :: com2
        character tmpc
        !logical, optional :: updatearg
        !logical, pointer :: comment
        logical :: comment = .false.
        !com2 = com
        !if (present(updatearg)) then
        !    if (updatearg) then
        !        comment => com
        !    else
        !        comment => com2
        !    end if
        !else
        !    comment => com2
        !end if
        tmp = line
        j = 0
        str = .false.
        group = 0
        skip = .false.
        start = 1
        do i=1, len(line)
            if (skip) then
                skip = .false.
                cycle
            end if
            if (line(i:i+1)=='*/') then
                if (comment) then
                    comment = .false.
                    skip = .true.
                    start = i+2
                    if (line(i+2:i+2)==' ') j = j - 1
                    cycle
                else
                    print '(I0, A)', lnum, ': closing comment has no beginning'
                    stop
                end if
            else if (comment) then
                cycle
            end if
            if (line(i:i+1)=='//') then
                tmp = tmp(:i-1)
                if (start==i) then
                    start = i+2
                    j = j - 1
                end if
                exit
            end if
            if (line(i:i+1)=='/*') then
                comment = .true.
                skip = .true.
                cycle
            end if
            tmpc = line(i:i)
            if (tmpc=='''') then
                str = .not.str
            else if (str.and.tmpc=='\') then
                skip = .true.
            else if (.not.str.and.group==0.and.(tmpc==' '.or.line(i:i+1)=='/*'.or.line(i:i+1)=='//')) then
                if (j==num) then
                    getop = trim(adjustl(line(start:i-1)))
                    return
                end if
                j = j + 1
                start = i+1
            end if
        end do
        if (j==num) then
            getop = trim(adjustl(tmp(start:)))
        else
            if (.not.present(error).or.error) print'(I0,A,I0,A)', lnum, ': missing operand (looking for operand ', num, ')'
            getop = ''
        end if
    end function

    subroutine c_parseDws(count,dwlist,pass)
        implicit none
        logical label
        integer, intent(out) :: count
        character(len=:), allocatable :: memsze, line, prevLine, temp, temp2, dwlist
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
        label = .false.
        count = 0
        memsze = ''
        comment = .false.
        dwlist = ''
      1 line = ''
      2 read (1, '(A)', advance='no', eor=3, end=4) readline
        line = line//readline
        goto 2
      3 line = line//readline
        call fixstr(line,comment)
        if (line(:7)=='@MEMSZE ') then
            memszet = strtype(getop(line,1))
            memsze = c_type(memszet)
            if (pass==2) write(2, '(A)') memsze//'* Dws;'
        else if (pass==1) then
            if (line(:1)=='.') then
                label = .true.
                prevLine = line
            else if (line(:3)=='DW '.or.line(:3)=='D8 '.or.line(:4)=='D16 '.or.line(:4)=='D32 '.or.line(:6)=='DADDR '.or.&
                & line(:6)=='DREAL '.or.line(:7)=='DLREAL ') then
                
            else if (getop(line,0,.false.)/='') then
                label = .false.
            end if
        end if
        if (line(:3)=='DW '.or.line(:3)=='D8 '.or.line(:4)=='D16 '.or.line(:4)=='D32 '.or.line(:6)=='DADDR '.or.&
        & line(:6)=='DREAL '.or.line(:7)=='DLREAL ') then
            if (memsze=='') then
                print'(I0,A)',lnum,': memsize must be declared before dw statments are used'
                stop
            end if
            if (label) then
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

program compile
    use helper
    implicit none
    type(variable), allocatable :: vars(:)
    type(variable) temp
    ! general
    integer i, dwcount
    ! argument parsing
    character(len=64) :: args, ifile, ofile
    logical :: output = .false.
    ! program parsing
    character(len=:), allocatable :: line, tmpstr, dwlist
    logical, target :: comment
    character(len=256) :: readline
    ! init vars
    allocate(vars(0))
    lnum = 0
    lnum2 = 0
    arch = 'C'
    memsze = 8
    comment = .false.
    ! get arguments
    ifile = 'input.urcl'
    ofile = 'output.c'
    do i=1, command_argument_count()
        call get_command_argument(i,args)
        if (args=='-o') then
            output = .true.
        else if (output) then
            ofile = args
        else
            ifile = args
        end if
    end do

    open(1, file=trim(ifile), action='read')
    open(2, file=trim(ofile), action='write')
    if (arch(:1)=='C') then
        call c_parseDws(dwcount, dwlist, 1)
        rewind 1
        call c_parseDws(dwcount, dwlist, 2)
        rewind 1
    end if
    call init()
  1 line = ''
  2 read (1, '(A)', advance='no', eor=3, end=9999) readline
    line = line//readline
    goto 2
  3 line = line//readline
    lnum = lnum + 1
    call fixstr(line, comment)
    tmpstr = getop(line,0,.false.)
    if (tmpstr(:1)=='.') then
        call label(line)
    else 
        if (arch(:1)=='C'.and.tmpstr/='') then
            lnum2 = lnum2 + 1
            write (2, '(A, I0, A)') 'line',lnum2,': ;'
        end if
        select case (tmpstr)
        case ('')
            continue
        case ('@VAR')
            call temp%create(strtype(getop(line,2)), getop(line,1))
            vars = [vars, temp]
        case ('@MEMSZE')
            memsze = strtype(getop(line,1))
            if (arch(:1)=='C') then
                write(2,'(A,I0,A)') 'Dws=malloc(',dwcount,'*sizeof('//c_type(memsze)//'));'
                write(2,'(A)') '#define const_SIZEMEM sizeof('//c_type(memsze)//')',dwlist
            end if
        case ('@MINMEM')
            call minmem(getop(line,1))
        case ('@DEFINE')
            tmpstr = getop(line,1)
            if (tmpstr(:1)/='@') then
                print'(I0,A)',lnum,': name of constant must start with @'
                stop
            end if
            if (arch(:1)=='C') then
                write(2,'(A)',advance='no') '#define const_'//tmpstr(2:)//' '
                tmpstr = parseArg(getop(line,2), i, vars)
                write(2,'(A)') tmpstr(2:)
            end if
        case ('@MINSTACK')
            call minstack(getop(line,1))
        case ('@MINCSTACK')
            call mincstack(getop(line,1))
        case ('LLOD',& !memory 
             &'ADD','SUB','MLT','DIV','MOD','BSL','BSR','BGE','BRG','BLE','BRL','BRE','BNE','BRC','BNC','AND','OR','XOR','NAND',& !unsigned
             &'NOR','XNOR','SETGE','SETG','SETLE','SETL','SETE','SETNE','SETC','SETNC',& !unsigned
             &'SDIV','SBSR','SBGE','SBRG','SBLE','SBRL','SBRC','SBNC','SSETGE','SSETG','SSETLE','SSETL','SSETC','SSETNC',& !signed
             &'FADD','FSUB','FMLT','FDIV','FMOD','FBGE','FBRG','FBLE','FBRL','FBRE','FBNE','FSETGE','FSETG','FSETLE','FSETL',& !real
             &'FSETE','FSETNE',& !real
             &'LFADD','LFSUB','LFMLT','LFDIV','LFMOD','LFBGE','LFBRG','LFBLE','LFBRL','LFBRE','LFBNE','LFSETGE','LFSETG','LFSETLE',& !long real
             &'LFSETL','LFSETE','LFSETNE')
             ! ABS, FABS, NEG, SRSH, MOV, INC, DEC, NOT, BRP, BRN, BRZ, BNZ, FBRZ, FBNZ, PSH, POP, CAL, RET, HLT
            call standard3Op(tmpstr, getop(line,1), getop(line,2), getop(line,3), vars)
        case ('MOV',& !general
             &'ABS','NEG','RSH','SRSH','SMOV','INC','DEC','NOT','BRP','BRN','BRZ','BNZ',& !integer
             &'FABS','FNEG','FBRP','FBRN','FBRZ','FBNZ',& !real
             &'LFABS','LFNEG','LFBRP','LFBRN','LFBRZ','LFBNZ',& !long real
             &'ITOF','FTOI','ITOLF','LFTOI','FTOLF','LFTOF') !conversion
            call standard2Op(tmpstr, getop(line,1), getop(line,2), vars)
        case ('JMP','CAL',& !control flow
             &'PSH','POP') !memory
            call standard1Op(tmpstr, getop(line,1), vars)
        case ('RET')
            call ret()
        case ('HLT')
            if (arch(:1)=='C') then
                write(2,'(A)') 'return 0;'
            else
                write(2,'(A)') 'HLT'
            end if
        case ('LSTR')
            call lstr(getop(line,1), getop(line,2), getop(line, 3), vars)
        case ('CPY') !CPY now takes an amount to copy (memcpy basically)
            call lstr(getop(line,1), getop(line,2), getop(line, 3), vars)
        case ('STR')
            call str(getop(line,1), getop(line,2), vars)
        case ('LOD')
            call lod(getop(line,1), getop(line,2), vars)
        case ('IMM')
            call imm(getop(line,1), getop(line,2), vars)
        case ('OUT')
            call out(getop(line,1), getop(line,2), vars)
        case ('IN')
            call in(getop(line,1), getop(line,2), vars)
        end select
    end if
    call updatecom(line, comment)
    goto 1
    9999 continue
    call end()
end
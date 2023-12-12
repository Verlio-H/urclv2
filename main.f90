module compilervars
    ! C  , U16, U32, SLK, IRI
    type DW
        character(len=:), allocatable :: label
        integer address
    end type
    type Defined
        character(len=:), allocatable :: name
        character(len=:), allocatable :: value
        logical :: int = .false.
        integer :: ivalue = 0
    end type
    character(len=3) :: arch
    integer :: memsze
    integer :: lnum
    integer :: lnum2
    type(DW), allocatable :: dws(:)
    type(Defined), allocatable :: defines(:)
    logical :: memdec = .false.
    logical :: stackdec = .false.
    logical :: cstackdec = .false.
    integer :: currentLoc = 1
    logical :: incprint32 = .false.
    logical :: incdiv32 = .false.
end

module var
    use compilervars
    implicit none
    logical, allocatable :: allocated(:)


    type variable
        character(len=:), allocatable :: name
        integer :: location = 0
        integer(kind=1) :: type = 0
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

    integer function evalConst(name)
        character(len=:), allocatable :: name
        integer :: i
        if (name(:1)=='@') then
            do i = 1, size(defines)
                if (defines(i)%name==name) then
                    if (defines(i)%int) then
                        evalConst = defines(i)%ivalue
                        return
                    else
                        print'(I0,A)',lnum,': constants must be integers in expressions'
                        evalConst = 0
                    end if
                end if
            end do
            print'(I0,A)',lnum,': defined constant was not defined'
        else if (name(:1)>='1'.and.name(:1)<='9') then
            read(name, *) evalConst
        else
            print'(I0,A)',lnum,': expected integer constant'
        end if
    end function

    function evalDefine(name)
        character(len=*) :: name
        character(len=:), allocatable :: evalDefine
        integer :: i
        if (name(:1)/='@') then
            print'(I0,A)',lnum,': internal error, expected defined constant'
            stop -1, quiet=.true.
        end if
        do i=1, size(defines)
            if (defines(i)%name==name) then
                evalDefine = defines(i)%value
                return
            end if
        end do
        print'(I0,A)',lnum,': invalid defined constant'
        stop -1, quiet=.true.
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
            stop -1, quiet=.true.
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
            stop -1, quiet=.true.
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
            stop -1, quiet=.true.
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
            stop -1, quiet=.true.
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
        else if (arch=='IRI') then
            write(2,'(A,I0)') '//var created at ',currentLoc
            this%type = int(type,1)
            this%name = trim(name)
            this%location = currentLoc
            currentLoc = currentLoc + 1
            if (type==32) currentLoc = currentLoc + 1
            return
        end if
        print '(A)', 'var creation not implemented for this target'
        stop -1, quiet=.true.
    end subroutine

    integer function var_get(this,arg)
        class(variable) this
        integer, optional :: arg
        if (.not.present(arg).and.arch=='IRI') then
            print '(A)', 'Internal error: arg required for var_get on Iris arch'
            stop -1, quiet=.true.
        end if
        if (arch=='IRI') then
            if (this%location<=19) then
                var_get = this%location
                return
            else
                select case (arg)
                case (3)
                    write(2,'(A,I0)') 'LOD R23 M',this%location-20
                    if (this%type==32) write(2,'(A,I0)') 'LOD R24 M',this%location-20
                    var_get = 23
                case (2)
                    write(2,'(A,I0)') 'LOD R21 M',this%location-20
                    if (this%type==32) write(2,'(A,I0)') 'LOD R22 M',this%location-20
                    var_get = 21
                case (1)
                    write(2,'(A,I0)') 'LOD R20 M',this%location-20
                    if (this%type==32) write(2,'(A,I0)') 'LOD R21 M',this%location-20
                    var_get = 20
                case default
                    print '(A)', 'invalid arg'
                    stop -1, quiet=.true.
                end select
                return
            end if
        else
            print '(A)', 'var get not implemented for this target'
            stop -1, quiet=.true.
        end if
    end function

    subroutine var_set(this, src, upper)
        class(variable) this
        character(len=*) :: src
        logical, optional :: upper
        integer :: upperActual
        character(len=11) :: strint
        upperActual = 0
        if (present(upper)) then
            if (upper) upperActual = 1
        end if
        if (arch(:1) == 'C') then
            write(2, '(A)') this%name//'='//src//';'
        else if (arch=='IRI') then
            if (this%location+upperActual<=19) then
                write(strint,'(I0)') this%location+upperActual
                if (index(src,' ')/=1) then
                    write(2,'(A)') src(:index(src,' '))//'R'//trim(strint)//src(index(src,' '):)
                else
                    write(2,'(A)') 'MOV R'//trim(strint)//src
                end if
            else
                write(strint,'(I0)') this%location+upperActual-20
                if (index(src,' ')/=1) then
                    write(2,'(A)') src(:index(src,' '))//'R25'//src(index(src,' '):)
                    write(2,'(A)') 'STR M'//trim(strint)//' R25'
                else
                    write(2,'(A)') 'STR M'//trim(strint)//src
                end if
            end if
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
                if (arg(:2)/='0X') then
                    read (arg, *) temp
                else
                    read (arg(3:),'(Z8)') temp
                    arg(2:2) = 'x'
                end if
                type = 8
                if (temp >= 256 .or. temp < -256) type = 16
                if (temp >= 65536 .or. temp < -65536) type = 32
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
            &'};'
            write(2,'(A)') 'int main() {',&
            &'union tmp tmp1, tmp2, tmp3;',&
            &'HEADER;'
        else if (arch=='IRI') then
            write(2,'(A)') 'BITS 16','MINREG 25'
        else
            print '(A)', 'init not implemented for this architecture'
        end if
    end subroutine

    subroutine end()
        character(len=256) :: readline
        if (arch(:1)=='C') then
            write(2,'(A)') '}'
        else if (arch=='IRI') then
            write(2,'(A)') 'HLT'
            if (incprint32) then
                open(3, file='print32.urcl', action='read')
                do
                    read(3, '(A)', end=1) readline
                    write(2, '(A)') trim(readline)
                end do
            end if
          1 close(3)
            if (incdiv32) then
                open(3, file='div32.urcl', action='read')
                do
                    read(3, '(A)', end=2) readline
                    write(2, '(A)') trim(readline)
                end do
            end if
          2 close(3)
        else
            print '(A)', 'end not implemented for this architecture'
        end if
    end subroutine

    subroutine label(arg1)
        character(len=:), allocatable :: arg1
        if (arch(:1)=='C') then
            write (2,'(A)') 'urcl'//arg1(2:)//': ;'
        else if (arch=='IRI') then
            write (2,'(A)') '.label_'//arg1(2:)
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
            write (2, '(A)') c_type(memsze)//' mem['//arg1//'];'
        else if (arch=='IRI') then
            if (memsze==32) then
                read (arg1, *) temp
                write (2, '(A,I0)') 'MINHEAP ',temp*2
            else
                write (2, '(A)') 'MINHEAP '//arg1
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
            write (2, '(A)') c_type(memsze)//' stack['//arg1//'];'
            write (2, '(A)') 'void* sp = stack;'
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
            write (2, '(A)') 'void* cstack['//arg1//'];'
            write (2, '(A)') 'int csp = 0;'
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
            if (addr<=19) then
                if (addr<0) addr=-addr
                if (type1==8.and.type2/=8) then
                    write(2,'(A,I0,A)') 'IMM R',addr,' '//result2//' 0xFF'
                else
                    write(2,'(A,I0,A)') 'IMM R',addr,' '//result2
                end if
            else
                if (type1==8.and.type2/=8) then
                    write(2,'(A)') 'IMM R21 '//result2//' 0xFF'
                    write(2,'(A,I0,A)') 'STR M',addr-20,' R21'
                else
                    write(2,'(A,I0,A)') 'STR M',addr-20,' '//result2
                end if
            end if
        else
            read(result2,*) int
            if (addr>0) then
                if (addr<=20) then
                    write(2,'(A,I0,A,I0)') 'IMM R',addr,' ',iand(int,2**16-1)
                else
                    write(2,'(A,I0,A,I0)') 'STR M',addr-21,' ',iand(int,2**16-1)
                end if
                if (addr<=19) then
                    write(2,'(A,I0,A,I0)') 'IMM R',addr+1,' ',shiftr(int,16)
                else
                    write(2,'(A,I0,A,I0)') 'STR M',addr-20,' ',shiftr(int,16)
                end if
            else
                print*,int
                write(2,'(A,I0,A,I0)') 'IMM R',-addr,' ',iand(int,2**16-1)
                write(2,'(A,I0,A,I0)') 'IMM R',-addr+1,' ',shiftr(int,16)
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
            write (2, '(A)') 'tmp1.v'//trim(typestr(type2))//'='//result2//';'
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
        character(len=:), allocatable :: output2u, output3u
        integer type1, type2, type3, itemp
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
            case ('BGE','BRE','BRL')
                if (type2/=32.and.type3/=32) then
                    call parseSmall(output1,result1,1,vars)
                    call parseSmall(output2,result2,2,vars)
                    call parseSmall(output3,result3,3,vars)
                    write (2,'(A)') inst//' '//output1//' '//output2//' '//output3
                else
                    call parseSmall(output1,result1,1,vars)
                    call parseBig(output2,output2u,result2,2,vars,type2)
                    call parseBig(output3,output3u,result3,3,vars,type3)
                    select case (inst)
                    case ('BGE')
                        if (output2u/=''.and.output3u/='') then
                            write (2,'(A)') 'BRG '//output1//' '//output2u//' '//output3u
                            write (2,'(A)') 'BNE ~+2 '//output2u//' '//output3u
                        else if (output3u=='') then
                            write (2,'(A)') 'BNZ '//output1//' '//output2u
                        else if (output2u=='') then
                            write (2,'(A)') 'BNZ ~+2 '//output3u
                        end if
                        write (2,'(A)') 'BGE '//output1//' '//output2//' '//output3
                    case ('BRE')
                        if (output2u/=''.and.output3u/='') then
                            write (2,'(A)') 'BNE ~+2 '//output2u//' '//output3u
                        else if (output3u=='') then
                            write (2,'(A)') 'BNE ~+2 '//output2u//' R0'
                        else if (output2u=='') then
                            write (2,'(A)') 'BNE ~+2 '//output3u//' R0'
                        end if
                        write (2,'(A)') 'BRE '//output1//' '//output2//' '//output3
                    case ('BRL')
                        if (output2u/=''.and.output3u/='') then
                            write (2,'(A)') 'BRL '//output1//' '//output2u//' '//output3u
                            write (2,'(A)') 'BNE ~+2 '//output2u//' '//output3u
                        else if (output3u=='') then
                            write (2,'(A)') 'BNZ ~+2 '//output2u
                        else if (output2u=='') then
                            write (2,'(A)') 'BNZ '//output1//' '//output3u
                        end if
                        write (2,'(A)') 'BRL '//output1//' '//output2//' '//output3
                    end select
                end if
            case ('ADD','SUB','DIV','MOD','NOR','AND')
                if (type1/=32) then
                    call parseSmall(output2,result2,2,vars)
                    call parseSmall(output3,result3,3,vars)
                    result1 = result1(2:)
                    call vars(getvar_index(vars,result1))%set(inst//' '//output2//' '//output3)
                else
                    call parseBig(output2,output2u,result2,2,vars,type2)
                    call parseBig(output3,output3u,result3,3,vars,type3)
                    result1 = result1(2:)

                    select case (inst)
                    case ('ADD')
                        if (output2u/=''.and.output3u/='') then
                            write (2,'(A)') 'ADD R25 '//output2u//' '//output3u
                        else if (output2u==''.and.output3u=='') then
                            write (2,'(A)') 'MOV R25 R0'
                        else if (output2u=='') then
                            write (2,'(A)') 'MOV R25 '//output3u
                        else if (output3u=='') then
                            write (2,'(A)') 'MOV R25 '//output2u
                        end if
                        write (2,'(A)') 'BNC ~+2 '//output2//' '//output3
                        write (2,'(A)') 'INC R25 R25'
                        call vars(getvar_index(vars,result1))%set(' R25',.true.)
                        call vars(getvar_index(vars,result1))%set('ADD '//output2//' '//output3)
                    case ('SUB')
                        write (2,'(A)') 'NOT R23 '//output3
                        write (2,'(A)') 'NOT R24 '//output3u
                        write (2,'(A)') 'BNC ~+2 R23 1'
                        write (2,'(A)') 'INC R24 R24'
                        write (2,'(A)') 'INC R23 R23'
                        write (2,'(A)') 'ADD R25 '//output2u//' R24'
                        write (2,'(A)') 'BNC ~+2 '//output2//' R23'
                        write (2,'(A)') 'INC R25 R25'
                        call vars(getvar_index(vars,result1))%set('ADD '//output2//' R23')
                        call vars(getvar_index(vars,result1))%set(' R25',.true.)
                    case ('DIV','MOD')
                        incdiv32 = .true.
                        itemp = vars(getvar_index(vars,result1))%location
                        if (itemp/=2.and.itemp/=3) then
                            write (2,'(A)') 'HPSH R3'
                        end if
                        if (itemp/=3.and.itemp/=4) then
                            write (2,'(A)') 'HPSH R4'
                        end if
                        if (itemp/=4.and.itemp/=5) then
                            write (2,'(A)') 'HPSH R5'
                        end if
                        if (itemp/=5.and.itemp/=6) then
                            write (2,'(A)') 'HPSH R6'
                        end if
                        write (2,'(A)') 'HPSH R7'
                        write (2,'(A)') 'HPSH R8'
                        write (2,'(A)') 'MOV R5 '//output2
                        if (output2u=='') then
                            write (2,'(A)') 'MOV R6 R0'
                        else
                            write (2,'(A)') 'MOV R6 '//output2u
                        end if
                        write (2,'(A)') 'MOV R7 '//output3
                        if (output3u=='') then
                            write (2,'(A)') 'MOV R8 R0'
                        else
                            write (2,'(A)') 'MOV R8 '//output3u
                        end if
                        write (2,'(A)') 'HCAL .div32'
                        write (2,'(A)') 'HPOP R8'
                        write (2,'(A)') 'HPOP R7'
                        if (inst=='DIV') then
                            call vars(getvar_index(vars,result1))%set(' R3')
                            call vars(getvar_index(vars,result1))%set(' R4',.true.)
                        else
                            call vars(getvar_index(vars,result1))%set(' R5')
                            call vars(getvar_index(vars,result1))%set(' R6',.true.)
                        end if
                        if (itemp/=5.and.itemp/=6) then
                            write (2,'(A)') 'HPOP R6'
                        end if
                        if (itemp/=4.and.itemp/=5) then
                            write (2,'(A)') 'HPOP R5'
                        end if
                        if (itemp/=3.and.itemp/=4) then
                            write (2,'(A)') 'HPOP R4'
                        end if
                        if (itemp/=2.and.itemp/=3) then
                            write (2,'(A)') 'HPOP R3'
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
                    end select
                end if
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
                    stop -1, quiet=.true.
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
        character(len=:), allocatable :: output1, output2, output1u, output2u
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
            case ('MOV','INC')
                if (type1/=32) then
                    call parseSmall(output1,result1,2,vars)
                    call parseSmall(output2,result2,3,vars)
                    write(2,'(A)') inst//' '//output1//' '//output2
                else
                    call parseBig(output1,output1u,result1,2,vars,type1)
                    call parseBig(output2,output2u,result2,3,vars,type2)
                    select case (inst)
                    case ('MOV')
                        call vars(getvar_index(vars, result1))%set(' '//output2)
                        call vars(getvar_index(vars, result1))%set(' '//output2u,.true.)
                    case ('INC')
                        write (2,'(A)') 'BNC ~+2 '//output1//' 1'
                        if (output2u/='') then
                            call vars(getvar_index(vars, result1))%set('INC '//output2u,.true.)
                        else
                            call vars(getvar_index(vars, result1))%set(' 1',.true.)
                        end if
                        call vars(getvar_index(vars, result1))%set('INC '//output2)
                    end select
                end if
            case ('BRP','BNZ')
                call parseSmall(output1,result1,2,vars)
                if (type2/=32) then
                    call parseSmall(output2,result2,3,vars)
                    write(2,'(A)') inst//' '//output1//' '//output2
                else
                    call parseBig(output2,output2u,result2,3,vars,type2)
                    select case (inst)
                    case ('BRP')
                        write (2,'(A)') 'BRP '//output1//' '//output2u
                    case ('BNZ')
                        write (2,'(A)') 'BNZ '//output1//' '//output2
                        write (2,'(A)') 'BNZ '//output1//' '//output2u
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
        case ('JMP','CAL')
            if (type1/=1) then
                print'(I0,A)', lnum, ': arg1 of '//inst//' must be of type ADDR'
                stop -1, quiet=.true.
            end if
            if (.not.cstackdec) then
                print'(I0,A)', lnum, ': size of call stack must be declared before use of CAL'
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
                write (2,'(A)') 'goto *('//result1//');'
            case ('CAL')
                write (2,'(A,I0,A)') 'cstack[csp]=&&line',lnum2+1,';'
                write (2,'(A)') 'csp++;'
                write (2,'(A)') 'goto *('//result1//');'
            case ('PSH')
                tmp = memsze
                if (typesize(type1)>typesize(tmp)) tmp = type1
                write (2,'(A)') '*('//c_type(tmp)//'*)sp='//result1//';'
                write (2,'(A)') 'sp+=1+(sizeof('//c_type(tmp)//')-1)/sizeof('//c_type(memsze)//');'
            case ('POP')
                tmp = memsze
                if (typesize(type1)>typesize(tmp)) tmp = type1
                write (2,'(A)') 'sp-=1+(sizeof('//c_type(tmp)//')-1)/sizeof('//c_type(memsze)//');'
                write (2,'(A)') result1//'=*('//c_type(tmp)//'*)sp;'
            end select
        else if (arch=='IRI') then
            select case (inst)
            case ('CAL')
                call parseSmall(output1,result1,2,vars)
                write (2,'(A)') 'HCAL '//output1
            case ('JMP')
                call parseSmall(output1,result1,2,vars)
                write (2,'(A)') 'JMP '//output1
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
            write (2, '(A)') 'tmp1.v'//trim(typestr(type2))//'='//result2//';'
            write (2, '(A)') 'tmp2.v'//trim(typestr(type2))//'='//result2//';'
            write (2, '(A)') 'tmp3.v'//trim(typestr(type3))//'='//result3//';'
            write (2, '(A)') 'memcpy(tmp1.vADDR, tmp2.vADDR, tmp3.v32);'
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
            write (2,'(A)') result1//'=*('//c_type(type1)//'*)'//result2//';'
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
            write (2,'(A)') 'csp--;'
            write (2,'(A)') 'goto *cstack[csp];'
        else if (arch=='IRI') then
            write (2,'(A)') 'HRET'
        end if
    end subroutine

    subroutine out(arg1,arg2,vars)
        type(variable), allocatable :: vars(:)
        character(len=:), allocatable :: arg1, arg2
        character(len=:), allocatable :: result1, result2, output2, output2u
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
                write(2,'(A)') 'printf("%c",'//result2//');'
            case ('%NUMB')
                write(2,'(A)') 'printf("%d",'//result2//');'
            case default
                print'(I0,A)', lnum, ': unknown port "'//result2//'" for target C'
            end select
        else if (arch=='IRI') then
            select case (result1)
            case ('%TEXT')
                call parseSmall(output2,result2,2,vars)
                write(2,'(A)') 'OUT %TEXT '//output2
            case ('%NUMB')
                if (type2/=32) then
                    call parseSmall(output2,result2,2,vars)
                    write(2,'(A)') 'OUT %NUMB '//output2
                else
                    call parseBig(output2,output2u,result2,2,vars,type2)
                    incprint32 = .true.
                    if (output2(:1)=='R') then
                        write(2,'(A)') 'BNZ ~+3 '//output2u,'OUT %NUMB '//output2
                        if (output2=='R1') then
                            write(2,'(A)') 'JMP ~+6','HPSH R2','HPSH R1','HCAL .print32','HPOP R1','HPOP R2'
                        else
                            write(2,'(A)') 'JMP ~+8','HPSH R2','HPSH R1'
                            write(2,'(A)')'MOV R1 '//output2, 'MOV R2 '//output2u
                            write(2,'(A)') 'HCAL .print32','HPOP R1','HPOP R2'
                        end if
                    else
                        write(2,'(A)') 'HPSH R2','HPSH R1'
                        write(2,'(A)')'IMM R1 '//output2, 'IMM R2 '//output2u
                        write(2,'(A)') 'HCAL .print32','HPOP R1','HPOP R2'
                    end if
                end if
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
                else if (line(i:i)=='['.or.line(i:i)==']') then
                    line(i:i) = ' '
                end if
            else if (tmp==2.and.(line(i:i)==''''.or.line(i:i)=='"')) then
                tmp = 0
            end if
        end do

        i = 1
        tmp = len(line)
        do while ((i+1)<=tmp)
            if (line(i:i+1)=='//') then
                line = line(:i-1)
                exit
            end if
            if (line(i:i+1)=='/*'.and.line(i-1:i-1)/=' ') then
                line = line(:i-1)//' /*'//line(i+2:)
                i = i+2
                tmp = tmp + 1
            end if
            i = i + 1
        end do
        comment = .false.
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
                        write(2,'(A)') 'DW '''//arg(j:j+1)//''''
                        skip = .true.
                    else
                        write(2,'(A)') 'DW '''//arg(j:j)//''''
                    end if
                    if (typedw==32) write(2,'(A)') 'DW 0'
                end do
            else
                result1 = parseArg(arg, type1, vars)
                if (typedw/=32) then
                    write(2,'(A)') 'DW '//result1(2:)
                else
                    read(result1(2:),*) temp
                    write(2,'(A,I0)') 'DW ',iand(temp,2**16-1)
                    write(2,'(A,I0)') 'DW ',shiftr(temp,16)
                end if
            end if
            i = i + 1
            arg = getop(line,i,.false.)
        end do
    end subroutine

    subroutine updatecom(line, comment)
        character(len=:), allocatable :: line
        logical comment, skip
        integer i
        skip = .false.
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
        logical istr, skip
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
        istr = .false.
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
                    stop -1, quiet=.true.
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
                istr = .not.istr
            else if (istr.and.tmpc=='\') then
                skip = .true.
            else if (.not.istr.and.group==0.and.(tmpc==' '.or.line(i:i+1)=='/*'.or.line(i:i+1)=='//')) then
                if (start>i) then
                    cycle
                end if
                if (j==num) then
                    getop = trim(adjustl(line(start:i-1)))
                    return
                end if
                j = j + 1
                start = i+1
                do while (line(start:start)==' '.and.start/=len(line))
                    start = start + 1
                end do

            end if
        end do
        if (j==num) then
            getop = trim(adjustl(tmp(start:)))
        else
            getop = ' '
            if (.not.present(error)) then
                print'(I0,A,I0,A)', lnum, ': missing operand (looking for operand ', num, ')'
                stop -1, quiet=.true.
            else if (error) then
                print'(I0,A,I0,A)', lnum, ': missing operand (looking for operand ', num, ')'
                stop -1, quiet=.true.
            end if
            
        end if
    end function

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
            if (pass==2) write(2, '(A)') dwmemsze//'* Dws;'
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
    logical :: archarg = .false.
    ! program parsing
    character(len=:), allocatable :: line, tmpstr, dwlist
    logical, target :: comment
    character(len=256) :: readline
    ! init vars
    allocate(vars(0))
    allocate(defines(6))
    lnum = 0
    lnum2 = 0
    arch = 'IRI'
    memsze = 8
    comment = .false.
    ! get arguments
    ifile = 'input.urcl'
    if (arch(:1)=='C') then
        ofile = 'output.c'
    else
        ofile = 'output.urcl'
    end if
    do i=1, command_argument_count()
        call get_command_argument(i,args)
        if (output) then
            ofile = args
            output = .false.
        else if (archarg) then
            if (args=='C') then
                arch = 'C'
                if (ofile == 'output.urcl') then
                    ofile = 'output.c'
                end if
            end if
            archarg = .false.
        else if (args=='-o') then
            output = .true.
        else if (args=='-arch') then
            archarg = .true.
        else
            ifile = args
        end if
    end do

    if (arch(:1)=='C') then
        defines(1)%name = '@SIZE8'
        defines(1)%value = '1'
        defines(1)%int = .true.
        defines(1)%ivalue = 1
        defines(2)%name = '@SIZE16'
        defines(2)%value = '2'
        defines(2)%int = .true.
        defines(2)%ivalue = 2
        defines(3)%name = '@SIZE32'
        defines(3)%value = '4'
        defines(3)%int = .true.
        defines(3)%ivalue = 4
        defines(4)%name = '@SIZEADDR'
        defines(4)%value = '8'
        defines(4)%int = .true.
        defines(4)%ivalue = 8
        defines(5)%name = '@SIZEREAL'
        defines(5)%value = '4'
        defines(5)%int = .true.
        defines(5)%ivalue = 4
        defines(6)%name = '@SIZELREAL'
        defines(6)%value = '8'
        defines(6)%int = .true.
        defines(6)%ivalue = 8
    else if (arch=='IRI') then
        defines(1)%name = '@SIZE8'
        defines(1)%value = '1'
        defines(1)%int = .true.
        defines(1)%ivalue = 1
        defines(2)%name = '@SIZE16'
        defines(2)%value = '1'
        defines(2)%int = .true.
        defines(2)%ivalue = 1
        defines(3)%name = '@SIZE32'
        defines(3)%value = '2'
        defines(3)%int = .true.
        defines(3)%ivalue = 2
        defines(4)%name = '@SIZEADDR'
        defines(4)%value = '1'
        defines(4)%int = .true.
        defines(4)%ivalue = 1
        defines(5)%name = '@SIZEREAL'
        defines(5)%value = '1'
        defines(5)%int = .true.
        defines(5)%ivalue = 1
        defines(6)%name = '@SIZELREAL'
        defines(6)%value = '1'
        defines(6)%int = .true.
        defines(6)%ivalue = 1
    end if

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
    if (len(tmpstr)>=1.and.tmpstr(:1)=='.') then
        call label(line)
    else 
        if (arch(:1)=='C'.and.(tmpstr/=''.and.tmpstr(:1)/='@')) then
            lnum2 = lnum2 + 1
            write (2, '(A, I0, A)') 'line',lnum2,': ;'
        else if (arch=='IRI'.and.(tmpstr/=''.and.tmpstr(:1)/='@')) then
            select case (tmpstr)
            case ('D8','D16','D32','DREAL','DLREAL','DADDR','DW')
            case default
                lnum2 = lnum2 + 1
                write (2, '(A, I0)') '.line',lnum2
            end select
        end if
        line = trim(adjustl(line))
        if (line(:4)=='OUT%') then
            tmpstr = line(4:index(line,' ')-1)
            call out(tmpstr, getop(line,1), vars)
            goto 9
        else if (line(:3)=='IN%') then
            tmpstr = line(4:index(line,' ')-1)
            call in(tmpstr, getop(line,1), vars)
            goto 9
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
            call define(getop(line,1),getop(line,2),vars)
            tmpstr = getop(line,1)
            if (tmpstr(:1)/='@') then
                print'(I0,A)',lnum,': name of constant must start with @'
                stop -1, quiet=.true.
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
             ! unsigned
             &'ADD','SUB','MLT','DIV','MOD','BSL','BSR','BGE','BRG','BLE','BRL','BRE','BNE','BRC','BNC','AND','OR','XOR','NAND',&
             &'NOR','XNOR','SETGE','SETG','SETLE','SETL','SETE','SETNE','SETC','SETNC',& !unsigned
             &'SDIV','SBSR','SBGE','SBRG','SBLE','SBRL','SBRC','SBNC','SSETGE','SSETG','SSETLE','SSETL','SSETC','SSETNC',& !signed
             &'FADD','FSUB','FMLT','FDIV','FMOD','FBGE','FBRG','FBLE','FBRL','FBRE','FBNE','FSETGE','FSETG','FSETLE','FSETL',& !real
             &'FSETE','FSETNE',& !real
             ! long real
             &'LFADD','LFSUB','LFMLT','LFDIV','LFMOD','LFBGE','LFBRG','LFBLE','LFBRL','LFBRE','LFBNE','LFSETGE','LFSETG','LFSETLE',&
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
        case ('DW','D8','D16','D32','DADDR','DREAL','DLREAL')
            if (arch=='IRI') then
                call data(tmpstr,line,vars)
            else if (arch(:1)/='C') then
                print'(I0,A)',lnum,': dw not implemented for this architecture'
            end if
        case default
            print'(I0,A)',lnum,': unknown instruction'
        end select
    end if
  9 call updatecom(line, comment)
    goto 1
    9999 continue
    call end()
end
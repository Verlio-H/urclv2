! TODO: Implement specified integer type for C
! TODO: Optimize IRIS 32 bit SUB
! TODO: Implement constant folding
! TODO: IRIS Microoptimizations to:
!  MLT by 1
!  MLT by power of 2
!  MLT by 0
!  MLT by -1
!  FMLT/LFMLT by 1
!  FMLT/LFMLT by 0
!  FMLT/LFMLT by -1
!  DIV by 1
!  DIV by power of 2
!  DIV by 0
!  FDIV/LFDIV by 1
!  FDIV/LFDIV by -1
!  ADD 1
!  SUB 1
!  ADD 0
!  SUB 0
!  BSR/BSL/BSS more than bitwidth
!  MOV/SMOV a a
!  MOD 1
! TODO: Throw warnings on:
!  DIV/MOD 0
!  integers in float operations
!  floats in integer operations


! Consts:
!  @SIZE8
!  @SIZE16
!  @SIZE32
!  @SIZEADDR
!  @SIZEREAL
!  @SIZELREAL
!  @FMAX
!  @FMIN
!  @LFMAX
!  @LFMIN
!  @MAX8
!  @MAX16
!  @MAX32
!  @SMAX8
!  @SMAX16
!  @SMAX32
!  @MSB8
!  @MSB16
!  @MSB32
!  @SMSB8
!  @SMSB16
!  @SMSB32
!  @BITSREAL
!  @BITSLREAL
!  @BITSADDR
!  @MINMEM
!  @MEMSZE
!  @MEM
!  @MEM8
!  @MEM16
!  @MEM32
!  @MEMADDR
!  @MEMREAL
!  @MEMLREAL

! Ports:
!  IN%TEXT ! input char
!  OUT%TEXT ! output char
!  OUT%NUMB ! output number
!  OUT%PIXEL ! output pixel to screen
!  IN%SIZEX ! get screen size x
!  IN%SIZEY ! get screen size y
!  IN%EXIT ! fetch any critical errors
module compilervars
    use includes, only: print32,div32,opengl
    ! C  , IRI
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
    type Trans
        character(len=:), allocatable :: name
        logical :: pure = .false.
        logical :: included = .false.
        logical :: c = .false.
        character(len=3), allocatable :: types(:)

        character(len=:), allocatable :: code
    end type
    character(len=3) :: arch
    integer :: memsze
    integer :: lnum
    integer :: lnum2
    integer :: unique = 0
    type(DW), allocatable :: dws(:)
    type(Defined), allocatable :: defines(:)
    logical :: memdec = .false.
    logical :: stackdec = .false.
    logical :: cstackdec = .false.
    integer :: currentLoc = 1
    logical :: incprint32 = .false.
    logical :: incdiv32 = .false.
    logical :: incopengl = .false.
    character(len=:), allocatable :: compiled
end

program compile
    use inst
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
    compiled = ''
  1 line = ''
  2 read (1, '(A)', advance='no', eor=3, end=9999) readline
    line = line//readline
    goto 2
  3 line = line//readline
    lnum = lnum + 1
    call fixstr(line, comment)
    tmpstr = getop(line,0,.false.)
    call app('//'//line)
    if (len(tmpstr)>=1.and.tmpstr(:1)=='.') then
        call label(line)
    else 
        if (arch(:1)=='C'.and.(tmpstr/=''.and.tmpstr(:1)/='@')) then
            lnum2 = lnum2 + 1
            
            call app('line'//itoa(lnum2)//': ;')
        else if (arch=='IRI'.and.(tmpstr/=''.and.tmpstr(:1)/='@')) then
            select case (tmpstr)
            case ('D8','D16','D32','DREAL','DLREAL','DADDR','DW')
            case default
                lnum2 = lnum2 + 1
                call app('.line'//itoa(lnum2))
            end select
        end if
        line = trim(adjustl(line))
        if (line(:4)=='OUT%') then
            tmpstr = line(4:index(line,' ')-1)
            call out(tmpstr, getop(line,1), getop(line,2,.false.), getop(line,3,.false.), vars)
            goto 9
        else if (line(:3)=='IN%') then
            tmpstr = line(3:index(line,' ')-1)
            call in(getop(line,1), tmpstr, vars)
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
                call app('Dws=malloc('//itoa(dwcount)//'*sizeof('//c_type(memsze)//'));')
                call app('#define const_SIZEMEM sizeof('//c_type(memsze)//')'//achar(10)//dwlist)
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
                call app('#define const_'//tmpstr(2:)//' ')
                tmpstr = parseArg(getop(line,2), i, vars)
                compiled = compiled//tmpstr(2:)
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
             &'ABS','NEG','LSH','RSH','SRSH','SMOV','INC','DEC','NOT','BRP','BRN','BRZ','BNZ',& !integer
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
                call app('return 0;')
            else
                call app('HLT')
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
    call init()
    write(2,'(A)') compiled
    call end()
end
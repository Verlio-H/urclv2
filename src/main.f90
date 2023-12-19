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
    type string
        character(len=:), allocatable :: value
    end type
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
    integer :: currframe = 1
    integer :: id

    type(string), allocatable :: args(:)
    type(string), allocatable :: parsed(:)
    integer, allocatable :: types(:)

    
end

program compiler
    use inst
    implicit none
    ! general
    integer i
    ! argument parsing
    character(len=64) :: arguments, ifile, ofile
    logical :: output = .false.
    logical :: archarg = .false.
    allocate(defines(6))

    lnum = 0
    lnum2 = 0
    arch = 'IRI'
    memsze = 8
    ! get arguments
    ifile = 'input.urcl'
    if (arch(:1)=='C') then
        ofile = 'output.c'
    else
        ofile = 'output.urcl'
    end if
    do i=1, command_argument_count()
        call get_command_argument(i,arguments)
        if (output) then
            ofile = arguments
            output = .false.
        else if (archarg) then
            if (arguments=='C') then
                arch = 'C'
                if (ofile == 'output.urcl') then
                    ofile = 'output.c'
                end if
            end if
            archarg = .false.
        else if (arguments=='-o') then
            output = .true.
        else if (arguments=='-arch') then
            archarg = .true.
        else
            ifile = arguments
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
    call instInit()
    !call compile(readFile('insts.urcl'))
    allocate(stack(1))
    allocate(stack(1)%args(0))
    allocate(stack(1)%parsed(0))
    allocate(stack(1)%types(0))
    args = stack(1)%args
    parsed = stack(1)%parsed
    types = stack(1)%types
    stack(1)%id = unique
    id = unique
    unique = unique + 1
    call compile()
    call init()
    write(2,'(A)') compiled
    call end()
contains
    recursive subroutine compile(input,initialvars)
        logical :: fromstr
        type(string), allocatable, optional :: input(:)
        type(variable), allocatable, intent(in), optional :: initialvars(:)

        character(len=:), allocatable :: line, tmpstr, dwlist
        logical :: comment, done
        integer :: dwcount
        type(variable), allocatable :: vars(:)
        type(variable) temp

        if (present(initialvars)) then
            vars = initialvars
        else
            allocate(vars(0))
        end if
        
        comment = .false.
        if (present(input)) then
            fromstr = .true.
        else
            fromstr = .false.
        end if

        compiled = ''

        if (.not.fromstr.and.arch(:1)=='C') then
            call c_parseDws(dwcount, dwlist, 1)
            rewind 1
            call c_parseDws(dwcount, dwlist, 2)
            rewind 1
        end if

        do
        if (fromstr) then
            if (.not.allocated(input)) return
            line = input(1)%value
            if (size(input)==1) then
                deallocate(input)
                done = .false.
            else
                done = .false.
                input = input(2:)
            end if
        else
            line = getline(done)
            lnum = lnum + 1
            if (mod(lnum,25000)==0) print*,lnum
        end if
        if (done) return
        call fixstr(line, comment)
        tmpstr = getop(line,0,.false.)
        if (.not.fromstr) call app('//'//line)
        if (len(tmpstr)>=1.and.tmpstr(:1)=='.') then
            call label(line)
        else 
            if (arch(:1)=='C'.and.(tmpstr/=''.and.tmpstr(:1)/='@')) then
                select case (tmpstr)
                case ('D8','D16','D32','DREAL','DLREAL','DADDR','DW')
                case default
                    lnum2 = lnum2 + 1
                    call app('line'//itoa(id)//'_'//itoa(lnum2)//': ;')
                end select
            else if (arch=='IRI'.and.(tmpstr/=''.and.tmpstr(:1)/='@')) then
                select case (tmpstr)
                case ('D8','D16','D32','DREAL','DLREAL','DADDR','DW')
                case default
                    lnum2 = lnum2 + 1
                    call app('.line'//itoa(id)//'_'//itoa(lnum2))
                end select
            end if
            line = trim(adjustl(line))
            if (line(:4)=='OUT%') then
                tmpstr = line(4:index(line,' ')-1)
                call out(tmpstr, getop(line,1), getop(line,2,.false.), getop(line,3,.false.), vars)
                goto 9
            else if (line(:3)=='IN%') then
                tmpstr = line(3:index(line,' ')-1)
                call in(getop(line,1), tmpstr, getop(line,2,.false.), getop(line,3,.false.), vars)
                goto 9
            end if
            if (transExists(tmpstr)) then
                call insertTrans(line,tmpstr,vars)
                cycle
            end if
            select case (tmpstr)
            case ('')
                continue
            case ('@INST')
                call inststart(getop(line,1))
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
                if (tmpstr(:1)/='@') call throw('name of constant must start with @')
    
            case ('@MINSTACK')
                call minstack(getop(line,1))
            case ('@MINCSTACK')
                call mincstack(getop(line,1))
            case ('LLOD',& !memory 
                 ! unsigned
                 &'ADD','SUB','MLT','DIV','MOD','BSL','BSR','BGE','BRG','BLE','BRL','BRE','BNE','BRC','BNC','AND','OR','XOR',&
                 &'NAND','NOR','XNOR','SETGE','SETG','SETLE','SETL','SETE','SETNE','SETC','SETNC',& !unsigned
                 !signed
                 &'SDIV','SBSR','SBGE','SBRG','SBLE','SBRL','SBRC','SBNC','SSETGE','SSETG','SSETLE','SSETL','SSETC','SSETNC',&
                 !real
                 &'FADD','FSUB','FMLT','FDIV','FMOD','FBGE','FBRG','FBLE','FBRL','FBRE','FBNE','FSETGE','FSETG','FSETLE','FSETL',&
                 &'FSETE','FSETNE',& !real
                 ! long real
                 &'LFADD','LFSUB','LFMLT','LFDIV','LFMOD','LFBGE','LFBRG','LFBLE','LFBRL','LFBRE','LFBNE','LFSETGE','LFSETG',&
                 &'LFSETLE','LFSETL','LFSETE','LFSETNE')
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
                    call throw('dw not implemented for this architecture')
                end if
            case default
                call throw('unknown instruction "'//tmpstr//'"',.false.)
            end select
        end if
      9 call updatecom(line, comment)
        end do
    end subroutine

    subroutine insertTrans(line,tmpstr,vars)
        character(len=:), allocatable :: line
        character(len=:), allocatable :: tmpstr
        type(variable), allocatable :: vars(:)
        type(trans) translation
        character(len=:), allocatable :: result
        integer :: type, j
        translation = getTrans(tmpstr,line,vars)
        if (.not.translation%compiled) then
            stack = [stack, frame(currentLoc=currentLoc, id=unique)]
            unique = unique + 1
            stack(currframe)%lnum2 = lnum2
            stack(currframe)%compiled = compiled
            stack(currframe)%currentLoc = currentLoc
            currframe = currframe + 1
            stack(currframe)%args = translation%argnames
            allocate(stack(currframe)%types(size(translation%types)))
            allocate(stack(currframe)%parsed(size(translation%types)))
            do i=1,size(translation%argnames)
                stack(currframe)%parsed(i)%value=parseArg(getop(line,i),stack(currframe)%types(i),vars)
            end do
            args = stack(currframe)%args
            parsed = stack(currframe)%parsed
            types = stack(currframe)%types
            id = stack(currframe)%id
            lnum2 = 0
            call compile(translation%code,vars)
            if (arch(:1)=='C') then
                compiled = stack(currframe-1)%compiled//achar(10)//'{'//compiled//achar(10)//'}'
            else
                compiled = stack(currframe-1)%compiled//compiled
            end if
            stack = stack(:currframe-1)
            currframe = currframe - 1
            lnum2 = stack(currframe)%lnum2
            types = stack(currframe)%types
            parsed = stack(currframe)%parsed
            args = stack(currframe)%args
            id = stack(currframe)%id
            currentLoc = stack(currframe)%currentLoc
        else
            if (arch(:1)=='C') call app('{')

            do i=1,size(translation%code)
                tmpstr = translation%code(i)%value
                do j=1,size(translation%argnames)
                    result = parseArg(getop(line,j),type,vars)
                    result = result(2:)
                    tmpstr = replace(tmpstr,translation%argnames(i)%value,result)
                end do
                call app(trim(tmpstr))
            end do
            if (arch(:1)=='C') call app('}')
        end if
    end subroutine
end
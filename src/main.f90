! TODO: Optimize IRIS 32 bit SUB
! TODO: Implement constant folding
! TODO: IRIS Microoptimizations to:
! TODO: Add in other compiler constants
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
    ! C   ,IRIS,SILK
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

    character(len=4) :: arch

    integer :: memsze

    integer :: lnum
    integer :: lnum2

    integer :: unique = 0

    type(Defined), allocatable :: defines(:)

    logical :: memdec = .false.
    logical :: stackdec = .false.
    logical :: cstackdec = .false.

    logical :: incprint32 = .false.
    logical :: incdiv32 = .false.
    logical :: incopengl = .false.
    logical :: incthread = .false.

    integer :: maximumLoc = 18
    logical :: inCalledTrans = .false.
    
    integer :: currframe = 1
    integer :: currentLoc = 1
    integer :: id
    character(len=:), allocatable :: compiled
    character(len=:), allocatable :: compiled2

    type(string), allocatable :: args(:)
    type(string), allocatable :: parsed(:)
    integer, allocatable :: types(:)

    character(len=:), allocatable :: savedCompiled
end

program compiler
    use emit
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
    arch = 'IRIS'
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
    else if (arch=='IRIS') then
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
    call insertCalledTrans()
    call init()
    if (arch(:1)=='C') then
        write(2,'(A)') compiled2
        write(2,'(A)') 'int run() {',&
        &'union tmp tmp1, tmp2, tmp3;',&
        &'HEADER;'
        write(2,'(A)') compiled
        write(2,'(A)') ending
        call end()
    else if (arch=='IRIS') then
        compiled = replacemem(compiled,max(maximumLoc-19,0))
        compiled2 = replacemem(compiled2,max(maximumLoc-19,0))
        write(2,'(A)') compiled
        call end()
        write(2,'(A)') compiled2
    end if
contains
    recursive subroutine compile(input,initialvars)
        logical :: fromstr
        type(string), allocatable, intent(inout), optional :: input(:)
        type(variable), allocatable, intent(in), optional :: initialvars(:)

        character(len=:), allocatable :: line, tmpstr, dwlist
        type(DW), allocatable :: dws(:)
        logical :: comment, done
        integer :: dwcount
        type(variable), allocatable :: vars(:)
        type(variable) temp

        if (present(initialvars)) then
            vars = initialvars
        else
            allocate(vars(0))
        end if


        if (present(input)) then
            fromstr = .true.
        else
            fromstr = .false.
        end if
        
        comment = .false.
        compiled = ''

        ending = ''
        dwlist = '' ! to shut up gfortran
        if (.not.fromstr.and.arch(:1)=='C') then
            dws = c_parseDws(dwcount, dwlist, 1)
            rewind 1
            dws = c_parseDws(dwcount, dwlist, 2, inputDws=dws)
            rewind 1
        else if (arch(:1)=='C') then
            dws = c_parseDws(dwcount, dwlist, 1, input)
            dws = c_parseDws(dwcount, dwlist, 2, input, dws)
            call app(c_type(memsze)//'* Dws'//itoa(id)//'=malloc('//itoa(dwcount)//'*'//itoa(typesize(memsze))//');')
            call app(dwlist)
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
            else if (arch=='IRIS'.and.(tmpstr/=''.and.tmpstr(:1)/='@')) then
                select case (tmpstr)
                case ('D8','D16','D32','DREAL','DLREAL','DADDR','DW')
                case default
                    lnum2 = lnum2 + 1
                    call app('.line'//itoa(id)//'_'//itoa(lnum2))
                end select
            end if
            line = trim(adjustl(line))
            if (transExists(tmpstr)) then
                call insertTrans(line,tmpstr,vars,dws,.false.)
                cycle
            end if
            if (line(:4)=='OUT%') then
                tmpstr = line(4:index(line,' ')-1)
                call out(tmpstr, getop(line,1), getop(line,2,.false.), getop(line,3,.false.), vars,dws)
                goto 9
            else if (line(:3)=='IN%') then
                tmpstr = line(3:index(line,' ')-1)
                call in(getop(line,1), tmpstr, getop(line,2,.false.), getop(line,3,.false.), vars,dws)
                goto 9
            else if (line(:5)=='ICAL%') then
                tmpstr = line(6:)
                call callinst(tmpstr,vars,dws)
                goto 9
            else if (line(:5)=='CCAL%') then
                tmpstr = line(6:)
                if(arch(:1)=='C') then
                    call callinst(tmpstr,vars,dws,.true.)
                else
                    call callinst(tmpstr,vars,dws)
                end if
                goto 9
            end if
            select case (tmpstr)
            case ('')
                continue
            case ('@INST')
                call inststart(getop(line,1),comment)
            case ('@VAR')
                call temp%create(strtype(getop(line,2)), getop(line,1))
                vars = [vars, temp]
            case ('@MEMSZE')
                memsze = strtype(getop(line,1))
                if (arch(:1)=='C') then
                    call app(c_type(memsze)//'* Dws'//itoa(id)//'=malloc('//itoa(dwcount)//'*'//itoa(typesize(memsze))//');')
                    call app('#define const_SIZEMEM sizeof('//c_type(memsze)//')'//achar(10)//dwlist)
                end if
            case ('@MINMEM')
                call minmem(getop(line,1))
            case ('@DEFINE')
                call define(getop(line,1),getop(line,2),vars,dws)
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
                call standard3Op(tmpstr, getop(line,1), getop(line,2), getop(line,3), vars,dws)
            case ('MOV',& !general
                 &'ABS','NEG','LSH','RSH','SRSH','SMOV','INC','DEC','NOT','BRP','BRN','BRZ','BNZ',& !integer
                 &'FABS','FNEG','FBRP','FBRN','FBRZ','FBNZ',& !real
                 &'LFABS','LFNEG','LFBRP','LFBRN','LFBRZ','LFBNZ',& !long real
                 &'ITOF','FTOI','ITOLF','LFTOI','FTOLF','LFTOF') !conversion
                call standard2Op(tmpstr, getop(line,1), getop(line,2), vars,dws)
            case ('JMP','CAL',& !control flow
                 &'PSH','POP') !memory
                call standard1Op(tmpstr, getop(line,1), vars,dws)
            case ('RET')
                call ret()
            case ('HLT')
                if (arch(:1)=='C') then
                    call app(ending)
                    if (inCalledTrans) then
                        call app('return;')
                    else
                        call app('return 0;')
                    end if
                else
                    if (inCalledTrans) then
                        call app('HRET')
                    else
                        call app('HLT')
                    end if
                end if
            case ('LSTR')
                call lstr(getop(line,1), getop(line,2), getop(line, 3), vars, dws)
            case ('CPY') !CPY now takes an amount to copy (memcpy basically)
                call lstr(getop(line,1), getop(line,2), getop(line, 3), vars, dws)
            case ('STR')
                call str(getop(line,1), getop(line,2), vars, dws)
            case ('LOD')
                call lod(getop(line,1), getop(line,2), vars, dws)
            case ('IMM')
                call imm(getop(line,1), getop(line,2), vars, dws)
            case ('DW','D8','D16','D32','DADDR','DREAL','DLREAL')
                if (arch=='IRIS') then
                    call data(tmpstr,line,vars,dws)
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

    subroutine insertTrans(line,name,vars,dws,called)
        character(len=:), allocatable, intent(in) :: line
        character(len=:), allocatable, intent(in) :: name
        type(DW), allocatable, intent(in) :: dws(:)
        type(variable), allocatable, intent(in) :: vars(:)
        logical, intent(in) :: called


        character(len=:), allocatable :: result
        type(trans) translation
        type(string), allocatable :: results(:,:)
        integer :: type, j

        translation = getTrans(name,line,vars,dws)
        if (.not.translation%compiled) then
            stack = [stack, frame(currentLoc=currentLoc, id=unique)]
            unique = unique + 1
            stack(currframe)%lnum2 = lnum2
            stack(currframe)%compiled = compiled
            stack(currframe)%currentLoc = currentLoc
            stack(currframe)%ending = ending
            currframe = currframe + 1
            stack(currframe)%args = translation%argnames
            allocate(stack(currframe)%types(size(translation%types)))
            allocate(stack(currframe)%parsed(size(translation%types)))
            do i=1,size(translation%argnames)
                stack(currframe)%parsed(i)%value=parseArg(getop(line,i),stack(currframe)%types(i),vars,dws)
                if (called.and.arch(:1)=='C'.and..not.translation%value(i)) then
                    stack(currframe)%parsed(i)%value = 'V*'//stack(currframe)%parsed(i)%value(2:)
                end if
            end do
            args = stack(currframe)%args
            parsed = stack(currframe)%parsed
            types = stack(currframe)%types
            id = stack(currframe)%id
            lnum2 = 0
            call compile(translation%code,vars)
            if (arch(:1)=='C') then
                compiled = stack(currframe-1)%compiled//achar(10)//'{'//compiled//achar(10)//ending//'}'
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
            ending = stack(currframe)%ending
            currentLoc = stack(currframe)%currentLoc
        else
            if (arch(:1)=='C') call app('{')
            if (arch(:1)=='C') then
                allocate(results(size(translation%argnames),1))
            else
                allocate(results(size(translation%argnames),2))
            end if

            do i=1,size(translation%argnames)
                result=parseArg(getop(line,i),type,vars,dws)
                if (arch=='IRIS') then
                    if (type==32) then
                        call parseBig(results(i,1)%value,results(i,2)%value,result,i,vars,type)
                    else
                        call parseSmall(results(i,1)%value,result,i,vars)
                        results(i,2)%value=''
                    end if
                else
                    results(i,1)%value=result(2:)
                end if
            end do
            do i=1,size(translation%code)
                result = translation%code(i)%value
                do j=1,size(translation%argnames)
                    if (arch=='IRIS') then
                        result = replace(result,translation%argnames(j)%value//'U',results(j,2)%value)
                    end if
                    result = replace(result,translation%argnames(j)%value,results(j,1)%value)
                end do
                call app(trim(result))
            end do
            if (arch(:1)=='C') call app('}')
        end if
    end subroutine


    subroutine insertCalledTrans()
        character(len=:), allocatable :: currLine, type
        type(DW), allocatable :: dws(:)
        type(variable), allocatable :: vars(:)
        type(variable) :: tmpvar
        integer :: idx, idx2
        savedCompiled = compiled
        compiled = ''
        compiled2 = ''
        do idx=1,size(translations)
            if (allocated(dws)) then
                deallocate(dws)
                allocate(dws(0))
            else
                allocate(dws(0))
            end if
            associate(this=>translations(idx))
                if (this%included) then
                    inCalledTrans = .true.
                    currLine = this%name
                    if (allocated(vars)) then
                        deallocate(vars)
                    end if
                    allocate(vars(size(this%argnames)))

                    if (arch=='IRIS') then
                        compiled2 = compiled2//'.inst_'//this%name//achar(10)
                    else if (arch(:1)=='C') then
                        compiled2 = compiled2//'void inst_'//this%name//'('
                    end if
                    currentLoc = maximumLoc + 1
                    do idx2=1,size(this%argnames)
                        type = this%types(idx2)%value(2:)
                        if (type=='*') call throw('called defined inst cannot have * as argument type')
                        if (arch=='IRIS') then
                            call tmpvar%create(strtype(type),this%argnames(idx2)%value)
                            savedCompiled = replace(&
                                savedCompiled,this%name//'$'//this%argnames(idx2)%value,'M'//itoa(tmpvar%location-19))
                            if (strtype(type)==32) then
                                savedCompiled = replace(&
                                    savedCompiled,this%name//'$$'//this%argnames(idx2)%value,'M'//itoa(tmpvar%location-18))
                            end if

                        else
                            tmpvar%name = this%argnames(idx2)%value
                            if (this%value(idx2)) then
                                tmpvar%ptr = .false.
                            else
                                tmpvar%ptr = .true.
                            end if
                            tmpvar%type = int(strtype(type),1)
                        end if
                        currLine = currLine//' '//tmpvar%name
                        vars(idx2) = tmpvar
                        if (arch(:1)=='C') then
                            if (tmpvar%ptr) then
                                compiled2 = compiled2//c_type(strtype(type))//'* '//this%argnames(idx2)%value
                            else
                                compiled2 = compiled2//c_type(strtype(type))//' '//this%argnames(idx2)%value
                            end if
                            if (idx2/=size(this%argnames)) compiled2 = compiled2//', '
                        end if
                    end do
                    if (arch(:1)=='C') then
                        compiled2 = compiled2//') {'//achar(10)//'union tmp tmp1, tmp2, tmp3;'//achar(10)
                    end if
                    call insertTrans(currLine,this%name,vars,dws,.true.)

                   compiled2 = compiled2//compiled//achar(10)
                    if (arch(:1)=='C') then
                        compiled2 = compiled2//'}'//achar(10)
                    else if (arch=='IRIS') then
                        compiled2 = compiled2//'HRET'//achar(10)
                    end if
                end if
                if (arch(:1)=='C'.and.this%concurrentIncluded) then
                    compiled2 = compiled2//'void cinst_'//this%name//'(void* args) {'//achar(10)
                    compiled2 = compiled2//'struct arguments {'//achar(10)
                    do idx2=1,size(this%argnames)
                        compiled2 = compiled2//c_type(strtype(this%types(idx2)%value(2:)))
                        if (.not.this%value(idx2)) compiled2 = compiled2//'*'
                        compiled2 = compiled2//' arg'//itoa(idx2)//';'//achar(10)
                    end do
                    compiled2 = compiled2//'};'//achar(10)//'inst_'//this%name//'('
                    do idx2=1,size(this%argnames)
                        compiled2 = compiled2//'((struct arguments*)args)->arg'//itoa(idx2)//','
                    end do
                    compiled2(len(compiled2):) = ')'
                    compiled2 = compiled2//';'//achar(10)//'}'//achar(10)
                end if
            end associate
        end do
        compiled = savedCompiled
    end subroutine
end
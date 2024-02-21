module inst
    use argparsing
    implicit none
    type Trans
        logical :: pure = .false.
        logical :: compiled = .false.
        logical :: included = .false.
        logical :: concurrentIncluded = .false.
        logical :: inserted = .false.
        logical :: concurrentInserted = .false.

        character(len=:), allocatable :: name
        type(string), allocatable :: argnames(:)
        type(string), allocatable :: types(:)
        type(Variable), allocatable :: globals(:)
        logical, allocatable :: value(:)

        type(string), allocatable :: code(:)
    end type
    type(Trans), allocatable :: translations(:)
    character(len=:), allocatable, private :: instName

! handling nesting
    type frame
        type(string), allocatable :: args(:)
        type(string), allocatable :: parsed(:)
        integer, allocatable :: types(:)
        character(len=:), allocatable :: compiled
        character(len=:), allocatable :: ending
        integer :: currentLoc
        integer :: lnum2 = 0
        integer :: id
    end type
    type(frame), allocatable :: stack(:)
    character(len=:), allocatable :: ending

contains

    subroutine instInit()
        allocate(translations(0))
    end subroutine

    subroutine callInst(line,vars,dws,concurrentOptional)
        character(len=:), allocatable, intent(in) :: line
        type(variable), allocatable, intent(in) :: vars(:)
        type(DW), allocatable, intent(in) :: dws(:)
        logical, intent(in), optional :: concurrentOptional
        logical :: concurrent

        type(Trans) :: translation

        character(len=:), allocatable :: result
        integer :: type, i
        character(len=:), allocatable :: output, outputu 

        if (present(concurrentOptional)) then
            concurrent = concurrentOptional
        else
            concurrent = .false.
        end if

        if (.not.transExists(getop(line,0))) call throw('no valid translation found for instruction '//getop(line,0))
        translation = getTrans(getop(line,0),line,vars,dws)
        i = getTrans_index(getop(line,0),line,vars,dws)
        translations(i)%included = .true.
        if (arch(:1)=='C') then
            if (concurrent) then
                output = 'struct struct'//itoa(unique)//' args'//itoa(unique)//' = (struct struct'//itoa(unique)//'){'
                call app('struct struct'//itoa(unique)//' {')
            else
                output = 'inst_'//getop(line,0)//'('
            end if
            do i=1,size(translation%argnames)
                result = parseArg(getop(line,i),type,vars,dws)
                if (concurrent) then
                    if (translation%value(i)) then
                        call app(c_type(strtype(translation%types(i)%value(2:)))//' arg'//itoa(i)//';')
                    else
                        call app(c_type(strtype(translation%types(i)%value(2:)))//'* arg'//itoa(i)//';')
                    end if
                end if
                if (result(:1)/='V'.and..not.translation%value(i)) then
                    call throw('instruction calls must not include immediates except for pass by value arguments')
                end if
                if (result(:1)=='V') then
                    result = result(2:)
                    if (vars(getvar_index(vars, result))%ptr) then
                        if (translation%value(i)) output = output//'*'
                    else
                        if (.not.translation%value(i)) output = output//'&'
                    end if
                    output = output//result
                else
                    result = result(2:)
                    output = output//result
                end if
                if (i/=size(translation%argnames)) output = output//', '
            end do
            if (concurrent) then
                call app('};')
                call app(output//'};')
                incthread = .true.
                call app('threads = realloc(threads,++sizethreads*sizeof(thrd_t));')
                call app(&
                 'thrd_create(threads+sizethreads-1,(thrd_start_t)cinst_'//getop(line,0)//',&args'//itoa(unique)//');')
                i = getTrans_index(getop(line,0),line,vars,dws)
                translations(i)%concurrentIncluded = .true.
            else
                call app(output//');')
            end if
        else
            do i=1,size(translation%argnames)
                result = parseArg(getop(line,i),type,vars,dws)
                if (result(:1)/='V'.and..not.translation%value(i)) then
                    call throw('instruction calls must not include immediates except for pass by value arguments')
                end if
                call parseBig(output,outputu,result,1,vars,type)
                call app('STR '//translation%name//'$'//translation%argnames(i)%value//' '//output)
                if (translation%types(i)%value(2:)=='32') then
                    if (outputu=='') outputu = 'R0'
                    call app('STR '//translation%name//'$$'//translation%argnames(i)%value//' '//outputu)
                end if
            end do
            call app('HCAL .inst_'//translation%name)
            do i=1,size(translation%argnames)
                result = parseArg(getop(line,i),type,vars,dws)
                result = result(2:)
                if (.not.translation%value(i)) then
                    call vars(getvar_index(vars,result))%set(' '//translation%name//'$'//translation%argnames(i)%value)
                    if (type==32) then
                        if (translation%types(i)%value(2:)=='32') then
                            call vars(getvar_index(vars,result))%set(&
                             ' '//translation%name//'$$'//translation%argnames(i)%value,.true.)
                        else
                            call vars(getvar_index(vars,result))%set(' R0',.true.)
                        end if
                    end if
                end if
            end do
        end if
    end subroutine

    logical function transExists(name)
        character(len=*), intent(in) :: name
        integer :: i
        do i=1,size(translations)
            if (translations(i)%name==name) then
                transExists = .true.
                return
            end if
        end do
        transExists = .false.
    end function
    
    subroutine inststart(name,comment,input)
        character(len=*), intent(in) :: name
        logical, intent(inout) :: comment
        type(string), allocatable, intent(inout), optional :: input(:)
        if (transExists(name)) call throw('instruction with this name already declared')
        instName = name
        parseinst: block
            character(len=:), allocatable :: line, instruction
            do
            if (present(input)) then
                if (.not.allocated(input)) call throw('unexpected EOF')
                line = input(1)%value
                if (size(input)==1) then
                    deallocate(input)
                else
                    input = input(2:)
                end if
            else
                line = getline()
                lnum = lnum + 1
            end if
            call fixstr(line,comment)
            instruction = getop(line,0,.false.)
            call updatecom(line,comment)
            if (instruction=='') cycle
            if (instruction(:1)/='@') call throw('unexpected executable statement in instruction definition')
            select case (instruction)
            case ('@TRANS')
                if (present(input)) then
                    call starttrans(line,comment,input)
                else
                    call starttrans(line,comment)
                end if
            case ('@ENDINST')
                exit parseInst
            case ('@ENDTRANS')
                call throw('expected @endinst')
            case ('@INST')
                call throw('@inst cannot be nested')
            case ('@VAR', '@DEFINE')
                call throw(instruction(2:)//' statements should only be in executable sections')
            case default
                call throw('unknown statement')
            end select
            end do
        end block parseInst
    end subroutine

    subroutine starttrans(arguments,comment,input)
        character(len=:), intent(in), allocatable :: arguments
        logical, intent(inout) :: comment
        type(string), allocatable, intent(inout), optional :: input(:)
        character(len=:), allocatable :: line, op, arg, tmpstr
        integer :: i, indx
        translations = [translations, Trans()]
        associate (this => translations(size(translations)))
        this%name = instName
        allocate(this%argnames(0))
        do i=1,256
            arg = getop(arguments,i,.false.)
            if (arg=='') exit
            this%argnames = [this%argnames, string(arg)]
        end do
        allocate(this%types(size(this%argnames)))
        allocate(this%value(size(this%argnames)))
        do i=1,size(this%argnames)
            this%types(i)%value='**'
            this%value(i)=.false.
        end do
        do
            if (present(input)) then
                if (.not.allocated(input)) call throw('unexpected EOF')
                line = input(1)%value
                if (size(input)==1) then
                    deallocate(input)
                else
                    input = input(2:)
                end if
            else
                line = getline()
                lnum = lnum + 1
            end if
            call fixstr(line, comment)
            op = getop(line,0,.false.)
            call updatecom(line,comment)
            if (op(:1)/='@') call throw('unexpected executable statement in translation definition')
            select case (op)
            case ('@START')
                allocate(this%code(0))
                do
                    if (present(input)) then
                        if (.not.allocated(input)) call throw('unexpected EOF')
                        line = input(1)%value
                        if (size(input)==1) then
                            deallocate(input)
                        else
                            input = input(2:)
                        end if
                    else
                        line = getline()
                        lnum = lnum + 1
                    end if
                    tmpstr = line
                    call fixstr(tmpstr,comment)
                    if (getop(tmpstr,0,.false.)=='@ENDTRANS') exit
                    call updatecom(line,comment)
                    this%code = [this%code, string(line)]
                end do
                return
            case ('@TYPES')
                do i=1,size(this%types)
                    arg = getop(line,i,.false.)
                    if (arg=='') call throw('not enough types provided')
                    if (index(arg,'_VAL')/=0) then
                        this%value(i) = .true.
                        arg = arg(:index(arg,'_VAL')-1)
                    end if
                    if ((arg(:1)/='V'.and.arg(:1)/='I'.and.arg(:1)/='*')) call throw('invalid type')
                    this%types(i)%value = arg
                end do
                if (getop(line,size(this%types)+1,.false.)/='') call throw('too many types provided')
            case ('@COMPILED', '@ARCH')
                indx = index(line,' ')
                if (arch(:1)=='C'.and.index(line(indx:),'C')==0.or.arch=='IRIS'.and.(index(line(indx:),'URCL')==0.and.&
                 index(line(indx:),'IRIS')==0)) then
                    translations = translations(:size(translations)-1)
                    if (present(input)) then
                        if (.not.allocated(input)) call throw('unexpected EOF')
                        line = input(1)%value
                        if (size(input)==1) then
                            deallocate(input)
                        else
                            input = input(2:)
                        end if
                    else
                        line = getline()
                        lnum = lnum + 1
                    end if
                    call fixstr(line,comment)
                    do while (getop(line,0,.false.)/='@ENDTRANS')
                        call updatecom(line,comment)
                        if (present(input)) then
                            if (.not.allocated(input)) call throw('unexpected EOF')
                            line = input(1)%value
                            if (size(input)==1) then
                                deallocate(input)
                            else
                                input = input(2:)
                            end if
                        else
                            line = getline()
                            lnum = lnum + 1
                        end if
                        call fixstr(line,comment)
                        if (getop(line,0,.false.)=='@ENDINST') then
                            call throw('erroneous @endinst, expected @endtrans')
                        else if (getop(line,0,.false.)=='@INST') then
                            call throw('@inst cannot be nested')
                        end if
                    end do
                    call updatecom(line,comment)
                    return
                end if
                if (op=='@COMPILED') this%compiled = .true.
            case ('@VAR', '@DEFINE')
                call throw(op(2:)//' statements should only be in executable sections')
            case ('@ENDTRANS')
                call throw('warning: no executable statements in translation',.false.)
                allocate(translations(size(translations))%code(1))
                translations(size(translations))%code(1)%value = ''
                return
            case ('@ENDINST')
                call throw('expected @endtrans')
            case ('@TRANS')
                call throw('@trans cannot be nested')
            case ('@INST')
                call throw('@inst cannot be within a translation block')
            case default
                call throw('unknown statement')
            end select
        end do

        end associate
    end subroutine

    type(Trans) function getTrans(name,input,vars,dws)
        character(len=:), intent(in), allocatable :: name
        character(len=:), intent(in), allocatable :: input
        type(variable), intent(in), allocatable :: vars(:)
        type(DW), allocatable, intent(in) :: dws(:)
        character(len=:), allocatable :: result
        integer :: type, i, ii
        big: do i=1,size(translations)
            ii = 1
            if (translations(i)%name/=name) cycle big
            do while (getop(input,ii,.false.)/='')
                if (size(translations(i)%types)<ii) cycle big
                result = parseArg(getop(input,ii),type,vars,dws)
                if (translations(i)%types(ii)%value(:1)/='*'.and.result(:1)/=translations(i)%types(ii)%value(:1)) cycle big
                if (translations(i)%types(ii)%value(2:)/='*'.and.translations(i)%types(ii)%value(2:)/=typestr(type)) cycle big
                ii = ii + 1
            end do
            if (size(translations(i)%types)>=ii) cycle big
            ! valid translation
            getTrans = translations(i)
            return
        end do big
        call throw('no valid translation found for instruction '//name)
    end function

    integer function getTrans_index(name,input,vars,dws)
        character(len=:), intent(in), allocatable :: name
        character(len=:), intent(in), allocatable :: input
        type(variable), intent(in), allocatable :: vars(:)
        type(DW), allocatable, intent(in) :: dws(:)
        character(len=:), allocatable :: result
        integer :: type, i, ii
        big: do i=1,size(translations)
            ii = 1
            if (translations(i)%name/=name) cycle big
            do while (getop(input,ii,.false.)/='')
                if (size(translations(i)%types)<ii) cycle big
                result = parseArg(getop(input,ii),type,vars,dws)
                if (translations(i)%types(ii)%value(:1)/='*'.and.result(:1)/=translations(i)%types(ii)%value(:1)) cycle big
                if (translations(i)%types(ii)%value(2:)/='*'.and.translations(i)%types(ii)%value(2:)/=typestr(type)) cycle big
                ii = ii + 1
            end do
            if (size(translations(i)%types)>=ii) cycle big
            ! valid translation
            getTrans_index = i
            return
        end do big
        call throw('no valid translation found')
    end function

end module
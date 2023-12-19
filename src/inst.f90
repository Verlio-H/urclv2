module inst
    use emit
    implicit none
    type Trans
        logical :: pure = .false.
        logical :: compiled = .false.
        logical :: included = .false.

        character(len=:), allocatable :: name
        type(string), allocatable :: argnames(:)
        type(string), allocatable :: types(:)
        type(Variable), allocatable :: globals(:)

        type(string), allocatable :: code(:)
    end type
    type(Trans), allocatable, private :: translations(:)
    character(len=:), allocatable, private :: instName

! handling nesting
    type frame
        type(string), allocatable :: args(:)
        type(string), allocatable :: parsed(:)
        integer, allocatable :: types(:)
        character(len=:), allocatable :: compiled
        integer :: currentLoc
        integer :: lnum2 = 0
        integer :: id
    end type
    type(frame), allocatable :: stack(:)


contains
    
    subroutine instInit()
        allocate(translations(0))
    end subroutine

    logical function transExists(name)
        character(len=*) :: name
        integer :: i
        do i=1,size(translations)
            if (translations(i)%name==name) then
                transExists = .true.
                return
            end if
        end do
        transExists = .false.
    end function
    
    subroutine inststart(name)
        character(len=*) :: name
        if (transExists(name)) call throw('instruction with this name already declared')
        instName = name
        parseinst: block
            character(len=:), allocatable :: line, instruction
            logical :: comment
            comment = .false.
            do
            line = getline()
            lnum = lnum + 1
            call fixstr(line, comment)
            instruction = getop(line,0,.false.)
            if (instruction=='') cycle
            if (instruction(:1)/='@') call throw('unexpected executable statement in instruction definition')
            select case (instruction)
            case ('@TRANS')
                call starttrans(line)
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

    subroutine starttrans(arguments)
        character(len=:), allocatable :: arguments, line, op, arg, tmpstr
        logical :: comment
        integer :: i
        comment = .false.
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
        do i=1,size(this%types)
            this%types(i)%value='**'
        end do
        do
        line = getline()
        lnum = lnum + 1
        call fixstr(line, comment)
        op = getop(line,0,.false.)
        if (op(:1)/='@') call throw('unexpected executable statement in translation definition')
        select case (op)
        case ('@START')
            allocate(this%code(0))
            do
            line = getline()
            lnum = lnum+1
            tmpstr = line
            call fixstr(tmpstr,comment)
            if (getop(tmpstr,0,.false.)=='@ENDTRANS') exit
            this%code = [this%code, string(line)]
            end do
            return
        case ('@TYPES')
            do i=1,size(this%types)
                arg = getop(line,i,.false.)
                if (arg=='') call throw('not enough types provided')
                if ((arg(:1)/='V'.and.arg(:1)/='I'.and.arg(:1)/='*')) call throw('invalid type')
                this%types(i)%value = arg
            end do
            if (getop(line,size(this%types)+1,.false.)/='') call throw('too many types provided')
        case ('@COMPILED')
            arg = getop(line,1)
            if (arch(:1)=='C') then
                if (arg/='C') then
                    translations = translations(:size(translations)-1)
                    line = getline()
                    lnum = lnum + 1
                    do while (getop(line,0,.false.)/='@ENDTRANS')
                        line = getline()
                        lnum = lnum + 1
                    end do
                    return
                end if
            else if (arch=='IRI') then
                if (arg/='URCL'.and.arg/='IRIS') then
                    translations = translations(:size(translations)-1)
                    line = getline()
                    call fixstr(line,comment)
                    lnum = lnum + 1
                    do while (getop(line,0,.false.)/='@ENDTRANS')
                        line = getline()
                        call fixstr(line,comment)
                        lnum = lnum + 1
                    end do
                    return
                end if
            end if
            this%compiled = .true.
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

    type(Trans) function getTrans(name,input,vars)
        character(len=:), intent(in), allocatable :: name
        character(len=:), intent(in), allocatable :: input
        type(variable), allocatable :: vars(:)
        character(len=:), allocatable :: result
        integer :: type, i, ii
        big: do i=1,size(translations)
            ii = 1
            if (translations(i)%name/=name) cycle big
            do while (getop(input,ii,.false.)/='')
                if (size(translations(i)%types)<ii) cycle big
                result = parseArg(getop(input,ii),type,vars)
                if (translations(i)%types(ii)%value(:1)/='*'.and.result(:1)/=translations(i)%types(ii)%value(:1)) cycle big
                if (translations(i)%types(ii)%value(2:)/='*'.and.translations(i)%types(ii)%value(2:)/=typestr(type)) cycle big
                ii = ii + 1
            end do
            if (size(translations(i)%types)>=ii) cycle big
            ! valid translation
            getTrans = translations(i)
            return
        end do big
        call throw('no valid translation found')
    end function
end module
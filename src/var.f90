module var
    use helper
    implicit none

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
                        call throw('constants must be integers in expressions',.false.)
                        evalConst = 0
                    end if
                end if
            end do
            call throw('defined constant "'//name//'" was not defined',.false.)
            evalConst = 0
        else if (name(:1)=='-'.or.name(:1)=='+'.or.(name(:1)>='1'.and.name(:1)<='9')) then
            read(name, *) evalConst
        else
            print*,name
            call throw('expected integer constant',.false.)
            evalConst = 0
        end if
    end function

    function evalDefine(name)
        character(len=*) :: name
        character(len=:), allocatable :: evalDefine
        integer :: i
        if (name(:1)/='@') call throw('internal error, expected defined constant')
        do i=1, size(defines)
            if (defines(i)%name==name) then
                evalDefine = defines(i)%value
                return
            end if
        end do
        call throw('invalid defined constant')
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
            call throw('unknown data type "'//input//'"')
            stop !never executed but here to stop gfortran from complaining
        end select
    end function

    function typestr(input)
        integer, intent(in) :: input
        character(len=5) :: typestr
        select case (input)
        case (64)
            typestr = '64'
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
            call throw('internal error, unknown type number '//itoa(input))
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
        case (64)
            c_type = 'unsigned long long'
        case default
            call throw('unsupported data type "'//itoa(type)//'" for c')
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
            call throw('unsupported data type "'//itoa(type)//'" for c')
        end select
    end function

    subroutine var_create(this, type, name)
        class(variable) this
        integer, intent(in) :: type
        character(len=*), intent(in) :: name
        if (arch(:1) == 'C') then
            call app(c_type(type)//' '//trim(name)//';')
            this%type = int(type, 1)
            this%name = trim(name)
            return
        else if (arch=='IRI') then
            call app('//var created at '//itoa(currentLoc))
            this%type = int(type,1)
            this%name = trim(name)
            this%location = currentLoc
            currentLoc = currentLoc + 1
            if (type==32) currentLoc = currentLoc + 1
            return
        end if
        call throw('var creation not implemented for this target')
    end subroutine

    integer function var_get(this,arg)
        class(variable) this
        integer, optional :: arg
        if (.not.present(arg).and.arch=='IRI') call throw('internal error: arg required for var_get on Iris arch')
        if (arch=='IRI') then
            if (this%location<18.or.this%location==18.and.this%type/=32) then
                var_get = this%location
                return
            else
                select case (arg)
                case (3)
                    if (this%location==18) then
                        call app('MOV R23 R18')
                    else
                        call app('LOD R23 M'//itoa(this%location-19))
                    end if
                    if (this%type==32) call app('LOD R24 M'//itoa(this%location-18))
                    var_get = 23
                case (2)
                    if (this%location==18) then
                        call app('MOV R21 R18')
                    else
                        call app('LOD R21 M'//itoa(this%location-19))
                    end if
                    if (this%type==32) call app('LOD R22 M'//itoa(this%location-18))
                    var_get = 21
                case (1)
                    if (this%location==18) then
                        call app('MOV R19 R18')
                    else
                        call app('LOD R19 M'//itoa(this%location-19))
                    end if
                    if (this%type==32) call app('LOD R20 M'//itoa(this%location-18))
                    var_get = 19
                case default
                    call throw('invalid arg number')
                    stop !never executed but here to stop gfortran from complaining
                end select
                return
            end if
        else
            call throw('var get not implemented for this target')
            stop !never executed but here to stop gfortran from complaining
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
            call app(this%name//'='//src//';')
        else if (arch=='IRI') then
            if (this%location+upperActual<=18) then
                write(strint,'(I0)') this%location+upperActual
                if (index(src,' ')/=1) then
                    call app(src(:index(src,' '))//'R'//trim(strint)//src(index(src,' '):))
                else
                    call app('MOV R'//trim(strint)//src)
                end if
            else
                write(strint,'(I0)') this%location+upperActual-19
                if (index(src,' ')/=1) then
                    call app(src(:index(src,' '))//'R25'//src(index(src,' '):))
                    call app('STR M'//trim(strint)//' R25')
                else
                    call app('STR M'//trim(strint)//src)
                end if
            end if
        else
            call throw('var set not implemented for this target')
        end if

    end subroutine

    subroutine var_remove(this)
        class(variable) this
        if (arch(:1) == 'C') then
            this%name=this%name !scuffed way to silence warning
            continue
        else
            call throw('var remove not implemented for this target')
        end if
    end subroutine
end module
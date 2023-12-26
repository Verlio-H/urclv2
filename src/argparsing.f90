module argparsing
    use var
    implicit none
contains
    subroutine parseSmall(result,input,arg,vars)
        character(len=:), allocatable, intent(out) :: result
        character(len=:), allocatable, intent(in) :: input
        integer, intent(in) :: arg
        type(variable), allocatable, intent(in) :: vars(:)
        integer :: index
        if (input(:1)=='V') then
            result = input(2:)
            index = vars(getvar_index(vars,result))%get(arg)
            result = repeat(' ', 11) ! This line can be removed in fortran 2023
            write(result,'(I0)') index
            result = 'R'//trim(adjustl(result))
        else
            result = input(2:)
        end if
    end subroutine

    subroutine parseBig(result,resultu,input,arg,vars,type)
        character(len=:), allocatable, intent(out) :: result, resultu
        character(len=:), allocatable, intent(in) :: input
        type(variable), allocatable, intent(in) :: vars(:)
        integer, intent(in) :: arg, type
        integer :: index, temp

        if (input(:1)=='V') then
            result = input(2:)
            index = vars(getvar_index(vars,result))%get(arg)
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
            result = input(2:)
            if (result(:1)/='M'.and.result(:1)/='.') then
                if (result(:2)/='0x') then
                    read(result,*) temp
                else
                    read(result(3:),'(Z8)') temp
                end if
                if (type==32.or.temp<0) then
                    result = repeat(' ',11)
                    write(result,'(I0)') iand(temp,2**16-1)
                    resultu = repeat(' ',11)
                    write(resultu,'(I0)') shiftr(temp,16)
                    result = trim(adjustl(result))
                    resultu = trim(adjustl(resultu))
                else
                    !result = input
                    resultu = ''
                end if
            else
                !result = input
                resultu = ''
            end if
        end if
    end subroutine

    recursive function parseArg(argInput, type, vars, dws) result(parse)
    
        character(len=:), allocatable, intent(in) :: argInput
        integer, intent(out) :: type
        type(variable), allocatable, intent(in) :: vars(:)
        type(DW), allocatable, intent(in) :: dws(:)
        character(len=:), allocatable :: parse, tmpstr, tmpstr2, arg
        type(variable) :: tmpvar
        integer temp, i
        arg = trim(argInput)
        if (arg(:1)=='@') then
            if (index(arg(2:),'+')==0) then
                temp = 0
            else
                temp = index(arg(2:),'+')+1
            end if
            if (temp==0.and.index(arg(2:),'-')/=0.or.(temp>index(arg(2:),'-')+1).and.index(arg(2:),'-')/=0) &
             temp = index(arg(2:),'-')+1
            if (temp==0.or.temp>index(arg,'*').and.index(arg,'*')/=0) temp = index(arg,'*')
            if (temp==0.or.temp>index(arg,'/').and.index(arg,'/')/=0) temp = index(arg,'/')
            tmpstr = arg(:temp-1)
            tmpstr2 = arg(temp+1:)
            if (temp==0) then
                if (tmpstr2(:1)=='@') then
                    tmpstr2 = parseArg(evalDefine(tmpstr2),type,vars,dws)
                end if
                parse = tmpstr2
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
        else if (arg(:1)=='%') then !port
            type = 4
            parse = arg
        else if (arg(:1)=='~') then !relative
            type = 1
            if (arch(:1)=='C') then
                read (arg(2:), *) temp
                allocate(character(len=12) :: parse)
                write (parse, *) lnum2+temp
                parse = 'I&&line'//itoa(id)//'_'//trim(adjustl(parse))
            else if (arch=='IRIS') then
                read (arg(2:), *) temp
                allocate(character(len=12) :: parse)
                write (parse, *) lnum2+temp
                parse = 'I.line'//itoa(id)//'_'//trim(adjustl(parse))
            end if
        else if (index(arg,'.')==0.and.(arg(:1)=='-'.or.arg(:1)=='+'.or.(arg(:1)>='0'.and.arg(:1)<='9'))) then !int
            !handle inline math
            if (index(arg(2:),'+')==0) then
                temp = 0
            else
                temp = index(arg(2:),'+')+1
            end if
            if (temp==0.and.index(arg(2:),'-')/=0.or.(temp>index(arg(2:),'-')+1).and.index(arg(2:),'-')/=0) &
             temp = index(arg(2:),'-')+1
            if (temp==0.or.temp>index(arg,'*').and.index(arg,'*')/=0) temp = index(arg,'*')
            if (temp==0.or.temp>index(arg,'/').and.index(arg,'/')/=0) temp = index(arg,'/')
            if (temp/=0) then

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
                    type = 8
                    if (temp >= 256 .or. temp < -256) type = 16
                    if (temp >= 65536 .or. temp < -65536) type = 32
                    parse = repeat(' ', 12)
                    write(parse,'(I12)') temp
                    parse = 'I'//trim(adjustl(parse))

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
                        parse = 'I(Dws'//itoa(id)//'+'//trim(adjustl(parse))//')'
                        return
                    end if
                end do
                parse = 'I&&'//'urcl'//itoa(id)//'_'//arg(2:)
            else
                parse = 'I.label'//itoa(id)//'_'//arg(2:)
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
                parse = 'I((void*)mem+'//arg(2:)//'*sizeof('//c_type(memsze)//'))'
            else
                if (memsze/=32) then
                    parse = 'I$$$'//arg(2:)
                else
                    read(arg(2:),*) temp
                    parse = 'I$$$'//itoa(temp*2)
                end if
            end if
        else !variable or arg
            do i=1,size(args)
                if (arg==args(i)%value) then
                    type = types(i)
                    parse = parsed(i)%value
                    return
                end if
            end do
            if (getvar_index(vars, arg)/=0) then
                tmpvar = getvar(vars,arg)
                type = tmpvar%type
                parse = 'V'//arg
            else
                call throw('unknown operand type '''//arg//''' (likely undeclared variable)')
                
            end if
        end if
    end function
end module
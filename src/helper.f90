
module helper
    use compilervars
    implicit none
   contains
    function readFile(fname)
        character(len=*), intent(in) :: fname
        type(string), allocatable :: readFile(:)
        logical :: done
        open(file=fname,unit=3,action='read')
        do
            readFile = [readFile, string()]
            readFile(size(readFile))%value = getline(done,3)
            ! readFile = [readFile, string(getline(done,3))] this threw an internal compiler error in gfortran lol
            if (done) exit
        end do
        close(3)
    end function

    pure function itoa(int)
        integer, intent(in) :: int
        character(len=:), allocatable :: itoa
        itoa = repeat(' ',12)
        write(itoa,'(I12)') int
        itoa = trim(adjustl(itoa))
    end function

    subroutine app(str)
        character(len=*), intent(in) :: str
        compiled = compiled//achar(10)//str
    end subroutine

    subroutine throw(err,stop)
        character(len=*) :: err
        logical, optional :: stop
        print'(I0,A)',lnum,': '//err
        if (present(stop)) then
            if (stop) then
                stop -1, quiet=.true.
            end if
        else
            stop -1, quiet=.true.
        end if
    end subroutine

    function getline(end,unit,debug) result(line)
        logical, intent(out), optional :: end
        integer, intent(in), optional :: unit
        integer, intent(in), optional :: debug
        integer :: unitActual
        character(len=:), allocatable :: line
        character(len=256) :: readline
        unitActual = 1
        if (present(unit)) unitActual = unit
        line = ''
      2 read (unitActual, '(A)', advance='no', eor=3, end=999) readline
        line = line//readline
        goto 2
      3 line = line//readline
        if (present(end)) end = .false.
        return
    999 if (present(end)) then
            end = .true.
        else
            if (present(debug)) then
                print'(A)','from line '//itoa(debug)
            end if
            call throw('unexpected EOF')
        end if
    end

    pure function replace(source,str1,str2)
        character(len=:), allocatable, intent(in) :: source
        character(len=*), intent(in) :: str1, str2
        character(len=:), allocatable :: replace
        integer :: i
        replace = ''
        i = 1
        do while (i<=len(source))
            if (source(i:i+len(str1)-1) == str1) then
                replace = replace//str2
                i = i + len(str1)
            else
                replace = replace//source(i:i)
                i = i + 1
            end if
        end do
    end function

    pure function replacemem(source,base)
        character(len=:), allocatable, intent(in) :: source
        integer, intent(in) :: base
        character(len=:), allocatable :: replacemem
        integer :: i, j, value
        replacemem = ''
        i = 1
        do while (i<=len(source))
            if (source(i:i+2)=='$$$') then
                j = i+3
                do while (source(j:j)>='0'.and.source(j:j)<='9') !might want to make sure not out of bounds
                    j = j + 1
                end do
                read(source(i+3:j-1),*) value
                replacemem = replacemem//'M'//itoa(value+base)
                i = j
            else
                replacemem = replacemem//source(i:i)
                i = i + 1
            end if
        end do

                
end function

    pure subroutine fixstr(line, comment)
        character(len=:), allocatable, intent(inout) :: line
        logical, intent(inout) :: comment
        integer tmp,i
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

    pure subroutine updatecom(line, comment)
        character(len=:), allocatable, intent(in) :: line
        logical, intent(out) :: comment
        logical :: skip
        integer :: i
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
        character(len=:), allocatable, intent(in) :: line
        integer, intent(in) :: num
        logical, intent(in), optional :: error
        character(len=:), allocatable :: getop, tmp
        integer i, j, group, start
        logical istr, skip, ilstr
        character tmpc
        logical :: comment
        comment = .false.
        tmp = line
        j = 0
        istr = .false.
        ilstr = .false.
        group = 0
        skip = .false.
        start = 1
        do i=1, len(line)
            if (skip) then
                skip = .false.
                cycle
            end if
            if (ilstr.and.tmpc=='\') then
                skip = .true.
                cycle
            else if(ilstr.and.line(i:i)/='"') then
                cycle
            else if (ilstr) then
                ilstr = .false.
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
                    call throw('closing comment has no beginning')
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
            else if (.not.istr.and.tmpc=='"') then
                ilstr = .true.
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
        if (.not.comment.and.j==num) then
            getop = trim(adjustl(tmp(start:)))
        else
            getop = ' '
            if (.not.present(error)) then
                call throw('missing operand (looking for operand '//itoa(num)//')')
            else if (error) then
                call throw('missing operand (looking for operand '//itoa(num)//')')
            end if
            
        end if
    end function
end module
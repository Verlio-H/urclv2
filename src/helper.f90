
module helper
    use compilervars
    implicit none
   contains

    function itoa(int)
        integer, intent(in) :: int
        character(len=:), allocatable :: itoa
        itoa = repeat(' ',12)
        write(itoa,'(I12)') int
        itoa = trim(adjustl(itoa))
    end function

    subroutine app(str)
        character(len=*) :: str
        compiled = compiled//achar(10)//str
    end subroutine

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
end module
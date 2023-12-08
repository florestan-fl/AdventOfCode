module m
    use iso_fortran_env
    implicit none
    character(len=:), allocatable :: input

contains 

subroutine getNum(str, int_res, i)
    character(:), allocatable, intent(in) :: str
    integer, intent(inout)                :: i
    integer, intent(out)                  :: int_res
    integer                               :: nb
    int_res = 0
    do i = i, len(str)
        if (str(i:i) >=  '0' .and. str(i:i) <= '9') then
            read(str(i:i),*) nb
            int_res = int_res * 10 + nb
        else
            exit
        endif
    enddo
endsubroutine

subroutine prettyPrint(maps, nbLines)
    integer, intent(in):: nbLines
    character(len=3), dimension(nbLines, 3), intent(in) :: maps
    integer :: i

    do i=1, nbLines
        write(*, '(a)', advance='no') maps(i, 1)
        write(*, '(a)', advance='no') ' -> ('
        write(*, '(a)', advance='no') maps(i, 2)
        write(*, '(a)', advance='no') ' | '
        write(*, '(a)', advance='no') maps(i, 3)
        write(*, '(a)', advance='no') ')'
        print *, ' '
    end do

end subroutine

subroutine isInferior(strA, strB, res)
    character(len=3), intent(in) :: strA, strB
    logical, intent(out) :: res
    integer :: i

    res = .false.
    do i=1,3
        if (strA(i:i) == strB(i:i)) cycle
        res = (strA(i:i) < strB(i:i))
        exit

    end do
end subroutine

subroutine sort(maps, nbLines)
    implicit none
    integer, intent(in) :: nbLines
    character(len=3), dimension(nbLines, 3), intent(inout) :: maps
    integer :: i, j
    character(len=3), dimension(3) :: x
    logical :: res

    x(:) = "---"
    do i = 2, nbLines
        x(1) = maps(i, 1)
        x(2) = maps(i, 2)
        x(3) = maps(i, 3)
        j = i - 1
        do while (j >= 1)
            call isInferior(x(1), maps(j, 1), res)
            res = .not.res
            if (res) exit
            maps(j + 1, 1) = maps(j, 1)
            maps(j + 1, 2) = maps(j, 2)
            maps(j + 1, 3) = maps(j, 3)
            j = j - 1
        end do
        maps(j + 1, 1) = x(1)
        maps(j + 1, 2) = x(2)
        maps(j + 1, 3) = x(3)
    end do
end subroutine

subroutine find(maps, str, nbLines, res)
    integer, intent(in) :: nbLines
    character(len=3), dimension(nbLines, 3), intent(in) :: maps
    integer, intent(out) :: res
    character(len=3) :: str
    integer :: start, ending, loc
    logical :: test
    start = 1
    ending = nbLines
    loc = (ending + start) /2
    do while (maps(loc, 1) /= str)
        call isInferior(maps(loc, 1), str, test)
        test = .not. test
        if (test) then
            ending = loc
            if (ending - start == 1) then
                loc = start
            else
                loc = (ending + start) /2
            end if
        else
            start = loc
            if (ending - start == 1) then
                loc = ending
            else
                loc = (ending + start) /2
            end if

        end if
    end do
    res = loc
end subroutine

subroutine problem()
    integer, parameter :: bufflen=1024
    integer :: io, isize, i, res, currLoc, instructionsLen
    character(len=:),allocatable  :: s
    character(len=bufflen) :: buffer
    character(:), allocatable :: instructions
    integer, parameter :: nbLines = 742
    character(len=3), dimension(nbLines, 3) :: maps
    logical eof

    res = 0
    open(UNIT=1, file=input, status='old')
    s = ''
    eof=.false.
    ! Read instructions line
    read(1,'(a)',advance='no', iostat=io, size=isize) buffer
    if(isize.gt.0)then
        s=s//buffer(:isize)
    endif
    print*, 's = ', s
    instructionsLen = isize
    instructions = s(:instructionsLen)

    ! Skip blank line
    read(1,'(a)',advance='no', iostat=io, size=isize) buffer

    i=1
readl:    do while(.not. eof)

        !Lexing part
        s=''
        read(1,'(a)',advance='no', iostat=io, size=isize) buffer
        if(isize.gt.0)then
            s=s//buffer(:isize)
        endif
        eof = io == iostat_end
        if (eof) exit
        print*, 's = ', s

        maps(i, 1) = s(1:3) ! src
        maps(i, 2) = s(8:11) ! L dest
        maps(i, 3) = s(13:15) ! R dest
        i = i + 1
    end do readl
    call sort(maps, nbLines)
    call prettyPrint(maps, nbLines)
    currLoc = 1 ! Array sorted so AAA is first
    i=1
    res = 0
    do while (maps(currLoc, 1) /= 'ZZZ')
        if (instructions(i:i) == 'L') then
            call find(maps, maps(currLoc, 2), nbLines, currLoc)
        else
            call find(maps, maps(currLoc, 3), nbLines, currLoc)
        end if
        i=i+1
        res = res + 1
        if (i > instructionsLen) i = 1
    end do
    

    print *, res
    close(1)
endsubroutine

endmodule

program main
    use m
    input='input.txt'
    call problem()
endprogram

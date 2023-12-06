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

subroutine problem()
    integer, parameter :: bufflen=1024
    integer :: io,  isize, res, i, j, k, temp, counter
    character(len=:),allocatable  :: s
    character(len=bufflen) :: buffer
    integer, parameter :: nbtimes = 3
    integer, dimension(nbtimes) :: times
    integer, dimension(:), allocatable :: speeds
    logical eof

    res = 0
    open(UNIT=1, file=input, status='old')
    s = ''
    eof=.false.

    !Lexing part
    s=''
    read(1,'(a)',advance='no', iostat=io, size=isize) buffer
    if(isize.gt.0)then
        s=s//buffer(:isize)
    endif
    print*, 's = ', s

    i = INDEX(s, ':') + 1
    do j=1, nbtimes
        do while (s(i:i) == ' ')
            i = i + 1
        end do
        call getNum(s, times(j), i)
    end do

    s=''
    read(1,'(a)',advance='no', iostat=io, size=isize) buffer
    if(isize.gt.0)then
        s=s//buffer(:isize)
    endif
    print*, 's = ', s

    i = INDEX(s, ':') + 1

    res=1
    do j=1, nbtimes
        counter = 0
        do while (s(i:i) == ' ')
            i = i + 1
        end do
        call getNum(s, temp, i)

        if (allocated(speeds)) deallocate(speeds)
        allocate(speeds(0:times(j)))
        speeds = [ (k, k = 1, times(j)) ]
        res = res * size(pack(speeds, (times(j)-speeds)*speeds>temp))
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

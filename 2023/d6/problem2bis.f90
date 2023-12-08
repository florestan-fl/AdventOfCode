! This is an alternative to my solution of problem2 that is not optimal at all but I wanted to do a dichotomy ok?
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

subroutine getTrimNum(str, int_res, i, amount)
    integer, parameter :: MyLongIntType = selected_int_kind (16)
    character(:), allocatable, intent(in) :: str
    integer, intent(inout)                :: i
    integer(kind=MyLongIntType), intent(out)                  :: int_res
    integer                               :: nb, j
    integer, intent(in)                   :: amount
    int_res = 0
loopamount:    do j=1, amount+1
        do while (str(i:i) == ' ')
            i = i + 1
        end do
        do i = i, len(str)
            if (str(i:i) >=  '0' .and. str(i:i) <= '9') then
                read(str(i:i),*) nb
                int_res = int_res * 10 + nb
            else if (str(i:i) == ' ') then
                cycle loopamount
            else
                exit
            endif
        end do
    enddo loopamount
endsubroutine

subroutine problem()
    integer, parameter :: bufflen=1024
    integer :: io,  isize, i, counter
    character(len=:),allocatable  :: s
    character(len=bufflen) :: buffer
    integer, parameter :: nbtimes = 4
    integer, parameter :: MyLongIntType = selected_int_kind (16)
    integer(kind=MyLongIntType) :: time, a, b, temp, start, ending, res
    logical eof

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
    do while (s(i:i) == ' ')
        i = i + 1
    end do
    call getTrimNum(s, time, i, nbtimes)

    s=''
    read(1,'(a)',advance='no', iostat=io, size=isize) buffer
    if(isize.gt.0)then
        s=s//buffer(:isize)
    endif
    print*, 's = ', s

    i = INDEX(s, ':') + 1

    res=1
    counter = 0
    do while (s(i:i) == ' ')
        i = i + 1
    end do
    call getTrimNum(s, temp, i, nbtimes)

    i=1
    a = 0
    do while(((time - a)*a<=temp).and.(a<=time))
        a=a+1
    end do
    b = (time + a+1)/2
    start = a+1
    ending = time
    do while(((time-b)*b-temp>=0).or.((time-b+1)*(b+1)-temp<=0))
        b = (ending+start)/2
        if ((time-b)*b-temp < 0) then
            ending = b
        else
            start = b
        end if
        i=i+1
    end do
    b = start
    res = b-a+1
    print *, res
    close(1)
endsubroutine

endmodule

program main
    use m
    input='input.txt'
    call problem()
endprogram

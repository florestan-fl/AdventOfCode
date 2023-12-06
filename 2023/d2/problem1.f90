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
    integer :: io,  isize, i, j, res, currId, temp
    character(len=:),allocatable  :: s
    character(len=bufflen) :: buffer
    integer, parameter :: red_max = 12
    integer, parameter :: green_max = 13
    integer, parameter :: blue_max = 14
    logical eof

    res = 0
    open(UNIT=1, file=input, status='old')
    s = ''
    eof=.false.
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

        i = 5
        do while (s(i:i) == ' ')
            i = i + 1
        end do
        call getNum(s, currId, i)
        ! Winning numbers
        i = i + 2
        do while (i < isize)
            do j=1, 3
                do while (s(i:i) == ' ')
                    i = i + 1
                end do
                call getNum(s, temp, i)
                i = i + 1
                if (s(i:i) == 'b') then
                    i = i + 4
                    if (temp > blue_max) then
                        cycle readl
                    end if
                else if (s(i:i) == 'r') then
                    i = i + 3
                    if (temp > red_max) then
                        cycle readl
                    end if
                else if (s(i:i) == 'g') then
                    i = i + 5
                    if (temp > green_max) then
                        cycle readl
                    end if
                end if
                if (s(i:i) == ';') then
                    i = i +2
                    exit
                end if
                i = i +2
            end do
        end do
        res = res + currId
    end do readl
    print *, res
    close(1)
endsubroutine

endmodule

program main
    use m
    input='input.txt'
    call problem()
endprogram

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
    integer :: io,  isize, i, j, res, currId, temp, temp_result
    character(len=:),allocatable  :: s
    character(len=bufflen) :: buffer
    integer, parameter :: winlen = 10
    integer, parameter :: chosenlen = 25
    integer, dimension(winlen) :: winning_numbers
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
        do j=1, winlen
            do while (s(i:i) == ' ')
                i = i + 1
            end do
            call getNum(s, temp, i)
            print *,temp
            winning_numbers(j) = temp
            i = i + 1
        end do

        ! vertical sep
        i = i + 2
        ! Choosen numbers

        temp_result = 0
        do j=1, chosenlen
            do while (s(i:i) == ' ')
                i = i + 1
            end do
            call getNum(s, temp, i)
            print *,temp
            if ( ANY( winning_numbers==temp )) then
                if (temp_result == 0) then
                    temp_result = 1
                else
                    temp_result = temp_result * 2
                end if
            end if
            i = i + 1
        end do

        res = res + temp_result

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

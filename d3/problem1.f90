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
    integer :: io,  isize, i, j, k, res, streak, temp, full_temp, e, f
    character(len=:),allocatable  :: s
    character(len=bufflen) :: buffer
    logical :: eof
    integer, parameter :: nblines=140
    integer, dimension(nblines, 140) :: array
    integer :: linewidth, last_added

    array(:,:) = 0
    res = 0
    open(UNIT=1, file=input, status='old')
    s = ''
    eof=.false.
    j = 1
readl:    do while(.not. eof)

        !Lexing part
        s=''
        read(1,'(a)',advance='no', iostat=io, size=isize) buffer
        if(isize.gt.0)then
            s=s//buffer(:isize)
            linewidth=isize
        endif
        eof = io == iostat_end
        if (eof) exit
        print*, 's = ', s

        streak = 0
        full_temp = 0
        do i=1, isize
            if (s(i:i) == '.') then
                array(j, i) = 0
                full_temp = 0
                streak = 0
            else if (s(i:i) >= '0' .and. s(i:i) <= '9') then
                read(s(i:i), '(I1)') temp
                full_temp = 10*full_temp + temp
                do k=i-streak, i
                    array(j, k) = full_temp
                end do
                streak = streak + 1
            else
                full_temp = 0
                array(j, i) = -1
                streak = 0
            end if
        end do
        j = j + 1
    end do readl

    last_added = 0
    do e=1, linewidth
        do f=1, nblines
            if (array(e,f)>0) then
                if (array(e,f) == last_added) then
                    cycle
                end if
                ! Upper and Left side
                ! Upper
                if (e-1 > 0) then
                    if (array(e-1,f) < 0) then
                        res = res + array(e, f)
                        last_added = array(e,f)
                    end if
                    ! Diag
                    if (f-1 > 0) then
                        if (array(e-1,f-1) < 0) then
                            res = res + array(e, f)
                            last_added = array(e,f)
                        end if
                    end if
                end if
                ! Left
                if (f-1 > 0) then
                    if (array(e,f-1) < 0) then
                        res = res + array(e, f)
                        last_added = array(e,f)
                    end if
                end if 

                ! Lower Left
                if (f-1 > 0 .and. e+1 <= linewidth) then
                    if (array(e+1,f-1) < 0) then
                        res = res + array(e, f)
                        last_added = array(e,f)
                    end if
                    ! Diag
                end if

                ! Lower and Right side
                ! Lower
                if (e+1 <= nblines) then
                    if (array(e+1,f) < 0) then
                        res = res + array(e, f)
                        last_added = array(e,f)
                    end if
                    ! Diag
                    if (f+1 <= linewidth) then
                        if (array(e+1,f+1) < 0) then
                            res = res + array(e, f)
                            last_added = array(e,f)
                        end if
                    end if
                end if
                ! Right
                if (f+1 <= linewidth) then
                    if (array(e,f+1) < 0) then
                        res = res + array(e, f)
                        last_added = array(e,f)
                    end if
                end if 

                ! Upper and Right side
                if (e-1 > 0 .and. f+1 <= linewidth) then
                    if (array(e-1,f+1) < 0) then
                        res = res + array(e, f)
                        last_added = array(e,f)
                    end if
                end if
            else
                last_added = 0
            end if
        end do
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

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
    integer :: linewidth
    integer :: fst_adj, scd_adj, nb_adj
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
            else if (s(i:i) == '*') then
                full_temp = 0
                array(j, i) = -2
                streak = 0
            else
                full_temp = 0
                array(j, i) = -1
                streak = 0
            end if
        end do
        j = j + 1
    end do readl

    fst_adj = 0
    scd_adj = 0
    nb_adj = 0
    do e=1, linewidth
        do f=1, nblines
            write(*, '(I3)', advance='no') array(e,f)
        end do
        print *, ''
    end do
    do e=1, linewidth
        do f=1, nblines
            if (array(e,f) == -2) then
                nb_adj = 0
                fst_adj = 0
                scd_adj = 0
                do i=max(0, e-1),min(nblines, e+1)
                    do j=max(0, f-1), min(linewidth, f+1)
                        if (i==e .and. j == f) then
                            cycle
                        end if
                        if (array(i,j) > 0 .and. array(i,j) /= fst_adj .and. array(i,j) /= scd_adj) then
                            nb_adj = nb_adj + 1
                            if (fst_adj == 0) then
                                fst_adj = array(i,j)
                            else if (scd_adj == 0) then
                                scd_adj = array(i,j)
                            end if
                        end if
                        
                    end do
                end do
                if (nb_adj == 2) then
                    res = res + fst_adj * scd_adj
                end if
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

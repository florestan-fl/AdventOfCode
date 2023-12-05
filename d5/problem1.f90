module m
    use iso_fortran_env
    implicit none
    character(len=:), allocatable :: input

contains 

subroutine getNum(str, int_res, i)
    integer, parameter :: MyLongIntType = selected_int_kind (16)
    character(:), allocatable, intent(in) :: str
    integer, intent(inout)                :: i
    integer(kind=MyLongIntType), intent(out)                  :: int_res
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

subroutine fillMap(map, length)
    integer, parameter :: MyLongIntType = selected_int_kind (16)
    integer, intent(inout)                :: length
    integer(kind=MyLongIntType), dimension(length, 3), intent(inout) :: map
    integer :: io,  isize, i, j
    character(len=:),allocatable :: s ! No intent
    integer, parameter :: bufflen=1024
    character(len=bufflen) :: buffer
    logical :: eof
    eof = .false.
    j = 1
    i = 1
    length = 0
read_map:    do while(.not. eof)

        !Lexing part
        s=''
        read(1,'(a)',advance='no', iostat=io, size=isize) buffer
        if(isize.gt.0)then
            s=s//buffer(:isize)
        endif
        eof = io == iostat_end
        if (eof) exit
        if (s == '') then
            exit
        end if
        print*, 's = ', s

        if (s(1:1) >= 'a' .and. s(1:1) <= 'z') then
            cycle
        end if

        i=1
        call getNum(s, map(j,1), i)
        i = i + 1
        call getNum(s, map(j,2), i)
        i = i + 1
        call getNum(s, map(j,3), i)
        j = j + 1
        length = length + 1
    end do read_map
endsubroutine

subroutine problem()
    integer, parameter :: MyLongIntType = selected_int_kind (16)
    integer, parameter :: bufflen=1024
    integer :: io,  isize, i, j
    integer(kind=MyLongIntType) :: res
    integer, parameter:: nbseeds = 20
    integer, parameter:: nbmaps = 65
    integer(kind=MyLongIntType), dimension(nbseeds) :: seeds
    integer(kind=MyLongIntType), dimension(nbmaps,3) :: to_soil, to_fert, to_water, to_light
    integer(kind=MyLongIntType), dimension(nbmaps,3) :: to_temp, to_humidity, to_loc
    integer :: len_to_soil, len_to_fert, len_to_water, len_to_light
    integer :: len_to_temp, len_to_humidity, len_to_loc, tmp_length
    character(len=bufflen) :: buffer
    logical :: eof
    character(len=:),allocatable :: s

    res = 0
    open(UNIT=1, file=input, status='old')
    s = ''
    eof=.false.
    s=''
    read(1,'(a)',advance='no', iostat=io, size=isize) buffer
    if(isize.gt.0)then
        s=s//buffer(:isize)
    endif
    eof = io == iostat_end
    print*, 's = ', s

    ! Read the seed
    i = 8
    j = 1
    do while (i <= isize)
        call getNum(s, seeds(j), i)
        j = j + 1
        i = i + 1
    end do

    ! skip line
    read(1,'(a)',advance='no', iostat=io, size=isize) buffer

    tmp_length = nbmaps
    call fillMap(to_soil, tmp_length)
    len_to_soil = tmp_length

    tmp_length = nbmaps
    call fillMap(to_fert, tmp_length)
    len_to_fert = tmp_length

    tmp_length = nbmaps
    call fillMap(to_water, tmp_length)
    len_to_water = tmp_length

    tmp_length = nbmaps
    call fillMap(to_light, tmp_length)
    len_to_light = tmp_length

    tmp_length = nbmaps
    call fillMap(to_temp, tmp_length)
    len_to_temp = tmp_length

    tmp_length = nbmaps
    call fillMap(to_humidity, tmp_length)
    len_to_humidity = tmp_length

    tmp_length = nbmaps
    call fillMap(to_loc, tmp_length)
    len_to_loc = tmp_length

    do i=1, nbseeds
        do j=1, len_to_soil
            if (seeds(i) >= to_soil(j,2) .and. seeds(i) < to_soil(j,2) + to_soil(j,3)) then
                seeds(i) = to_soil(j,1) + seeds(i) - to_soil(j,2)
                exit
            end if
        end do

        do j=1, len_to_fert
            if (seeds(i) >= to_fert(j,2) .and. seeds(i) < to_fert(j,2) + to_fert(j,3)) then
                seeds(i) = to_fert(j,1) + seeds(i) - to_fert(j,2)
                exit
            end if
        end do

        do j=1, len_to_water
            if (seeds(i) >= to_water(j,2) .and. seeds(i) < to_water(j,2) + to_water(j,3)) then
                seeds(i) = to_water(j,1) + seeds(i) - to_water(j,2)
                exit
            end if
        end do

        do j=1, len_to_light
            if (seeds(i) >= to_light(j,2) .and. seeds(i) < to_light(j,2) + to_light(j,3)) then
                seeds(i) = to_light(j,1) + seeds(i) - to_light(j,2)
                exit
            end if
        end do

        do j=1, len_to_temp
            if (seeds(i) >= to_temp(j,2) .and. seeds(i) < to_temp(j,2) + to_temp(j,3)) then
                seeds(i) = to_temp(j,1) + seeds(i) - to_temp(j,2)
                exit
            end if
        end do

        do j=1, len_to_humidity
            if (seeds(i) >= to_humidity(j,2) .and. seeds(i) < to_humidity(j,2) + to_humidity(j,3)) then
                seeds(i) = to_humidity(j,1) + seeds(i) - to_humidity(j,2)
                exit
            end if
        end do

        do j=1, len_to_loc
            if (seeds(i) >= to_loc(j,2) .and. seeds(i) < to_loc(j,2) + to_loc(j,3)) then
                seeds(i) = to_loc(j,1) + seeds(i) - to_loc(j,2)
                exit
            end if
        end do
    end do

    do i=1, nbseeds
    print *, seeds(i)
    end do
    res = seeds(1)
    do i=1, nbseeds
        if (seeds(i) < res) then
            res = seeds(i)
        end if
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

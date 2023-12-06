program problem1
    implicit none
    integer :: counter
    character(len=1000), allocatable :: line
    character :: temp
    integer :: i
    integer :: fst, last
    integer :: stat
    line = "start"

    counter = 0
    open (unit = 2, file = "input.txt", status='old') 
    fst = -1
    last = -1
    do while (line /= "")
        read(2, '(A)', iostat=stat) line
        if (stat < 0) then
            exit
        endif
        do i=1,len(line)
            temp = line(i:i)
            if (temp >= '0' .and. temp <= '9') then
                if (fst == -1 .or. last == -1) then
                    read(temp, '(I1)') fst
                    last = fst
                endif
                read(temp, '(I1)') last
            endif
        end do
        counter = counter + 10*fst + last
        fst = -1
        last = -1
    end do
    print '(I0)', counter 


end program

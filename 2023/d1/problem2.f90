program problem1
    implicit none
    integer :: counter
    character(len=1000), allocatable :: line
    character :: temp
    integer :: i
    integer :: fst, last
    integer :: stat
    character(len=3), allocatable :: three_letters_temp
    character(len=4), allocatable :: four_letters_temp
    character(len=5), allocatable :: five_letters_temp
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
            three_letters_temp = line(i:i+3)
            four_letters_temp = line(i:i+4)
            five_letters_temp = line(i:i+5)
            if (three_letters_temp == "one") then
                temp = '1'
            endif
            if (three_letters_temp == "two") then
                temp = '2'
            endif
            if (three_letters_temp == "six") then
                temp = '6'
            endif
            if (four_letters_temp == "zero") then
                temp = '0'
            endif
            if (four_letters_temp == "four") then
                temp = '4'
            endif
            if (four_letters_temp == "five") then
                temp = '5'
            endif
            if (four_letters_temp == "nine") then
                temp = '9'
            endif
            if (five_letters_temp == "three") then
                temp = '3'
            endif
            if (five_letters_temp == "seven") then
                temp = '7'
            endif
            if (five_letters_temp == "eight") then
                temp = '8'
            endif
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

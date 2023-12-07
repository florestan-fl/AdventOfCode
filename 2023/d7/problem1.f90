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

subroutine prettyPrint(hands, bids, nb_hands)
    integer, intent(in) :: nb_hands
    character(len=6), dimension(nb_hands), intent(in) :: hands
    integer, dimension(nb_hands), intent(in) :: bids
    integer :: i
    do i=1, nb_hands
        write(*, '(A)', advance='no') hands(i)
        write(*, '(A)', advance='no') ' '
    end do
    print *, ' '
    do i=1, nb_hands
        write(*, '(I4)', advance='no') bids(i)
        write(*, '(A)', advance='no') ' '
    end do
    print *, ' '
end subroutine

subroutine getPrefix(hand, res)
    ! High card: '0', pair: '1', two pairs: '2', three of a kind: '3',
    ! full house: '4', four of a kind: '5', five of a kind: '6'
    character(len=5), intent(in) :: hand
    character, intent(out) :: res
    integer :: i, j, k, l, nb, nb2, res2
    character :: comp, comp2

    res = '0'
    do i=1, 5
        nb=49 !'1'
        comp = hand(i:i)
        res2 = 1
        do j=i+1, 5
            if (hand(j:j) == comp) then
                nb = nb + 1
                if (nb == res2 .and. nb == 48 + 2) then
                    res = max(res, char(nb))
                else if ((nb==48+2 .and. res2==48+3).or.(nb==48+3 .and. res2==48+2)) then
                    res = max(res, char(48+4))
                end if
                cycle
            end if
            comp2 = ' '
            nb2=48 !'0' second pair or full house
            do k=j, 5
                if (hand(k:k) == comp) cycle
                comp2 = hand(k:k)
                nb2 = 49
                do l=k+1, 5
                    if (hand(l:l) == comp2) nb2 = nb2 + 1
                end do
                res2= max(res2, nb2)
            end do
            if (nb == res2 .and. nb == 48 + 2) then
                res = max(res, char(nb))
            else if ((nb==48+2 .and. res2==48+3).or.(nb==48+3 .and. res2==48+2)) then
                res = max(res, char(48+4))
            end if
        end do
        if (nb >= 48 + 4) nb = nb + 1
        if (nb <= 48 + 2) nb = nb - 1
        res = max(res, char(nb))
    end do

end subroutine

subroutine isInferior(strA, strB, res)
    character(len=6), intent(in) :: strA, strB
    logical, intent(out) :: res
    integer :: i
    character, dimension(5) :: letters

    res = .false.
    res = strA(1:1) < strB(1:1)
    if (strA(1:1) == strB(1:1)) then
        do i=2, 6
            if (strA(i:i) == strB(i:i)) cycle
            if (strA(i:i) >= '0' .and. strA(i:i) <= '9') then
                res = strB(i:i) >= strA(i:i) ! letters > numbers in ascii
                exit
            end if
            if (strB(i:i) >= '0' .and. strB(i:i) <= '9') then
                res = .false. ! strA is a letter and strB a number so smaller
                exit
            end if
            select case(strA(i:i))
                case ('T')
                    res = .true.
                case ('J')
                    res = strB(i:i) /= 'T'
                case ('Q')
                    res = (strB(i:i) /= 'T' .and. strB(i:i) /= 'J')
                case ('K')
                    res = strB(i:i) == 'A'
                case ('A')
                    res = .false.
            end select
            exit
        end do
    end if
end subroutine

subroutine sort(hands, bids, siz)
    implicit none
    integer, intent(in) :: siz
    character(len=6), dimension(siz), intent(inout) :: hands
    integer, dimension(siz),intent(inout) :: bids
    integer :: i, j, x_bids
    character(len=6) x
    logical :: res

    do i = 2, siz
        x = hands(i)
        x_bids = bids(i)
        j = i - 1
        do while (j >= 1)
            call isInferior(x, hands(j), res)
            res = .not.res
            if (res) exit
            hands(j + 1) = hands(j)
            bids(j + 1) = bids(j)
            j = j - 1
        end do
        hands(j + 1) = x
        bids(j + 1) = x_bids
    end do
end subroutine

subroutine sortInt(hands, siz)
    implicit none
    integer, intent(in) :: siz
    integer, dimension(siz), intent(inout) :: hands
    integer :: i, j
    integer x

    do i = 2, siz
        x = hands(i)
        j = i - 1
        do while (j >= 1)
            if (hands(j) < x) exit
            hands(j + 1) = hands(j)
            j = j - 1
        end do
        hands(j + 1) = x
    end do
end subroutine

subroutine problem()
    integer, parameter :: bufflen=1024
    integer :: io,  isize, i, j, res, temp
    character(len=:),allocatable  :: s
    character(len=bufflen) :: buffer
    integer, parameter :: nb_hands = 1000
    character(len=6), dimension(nb_hands) :: hands
    integer, dimension(nb_hands) :: bids
    logical eof, inferior
    character :: prefix

    res = 0
    hands(:) = "------"
    open(UNIT=1, file=input, status='old')
    s = ''
    eof=.false.
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

        call getPrefix(s(1:5), prefix)
        hands(i) = prefix // s(1:5)
        j=7
        call getNum(s, bids(i), j)
        i = i + 1
    end do readl
    call prettyPrint(hands, bids, nb_hands)
    call sort(hands, bids, nb_hands)
    !call sortInt(bids, nb_hands)
    call prettyPrint(hands, bids, nb_hands)
    
    do i=1, nb_hands
        res = res + i*bids(i)
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

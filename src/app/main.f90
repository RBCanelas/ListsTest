    program TestsList

    use list_mod
    use integer_list_mod
    use types_mod

    implicit none

    integer i
    type(list) :: simple_list
    type(integerList) :: int_list
    integer values(10)
    type(shape) :: square

    square%id=16
    square%filled=.true.

    !List
    do i=1, 10
        call simple_list%add(i)
    enddo

    print*, "From the list implementation"
    call simple_list%add(1.23)
    call simple_list%add('A')
    call simple_list%add('B')
    call simple_list%add('C')
    call simple_list%addValue(square)

    square%id=-8

    call simple_list%printvalues()

    !integer list (derived from the abstract list)
    do i=1, 10
        call int_list%add(i)
    enddo

    print*, "From the integer list (derived from the abstract list)"
    print*, "using printList()"
    call int_list%printList()
    print *

    call int_list%reset()
    i = 1
    do while(int_list%moreValues())
        values(i) = int_list%current()
        call int_list%next()
        i = i + 1
    end do

    print*, "Recovering the values to a variable and printing"
    print *, values

    end program TestsList

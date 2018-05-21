    !MARETEC - Research Centre for Marine, Environment and Technology
    !Copyright (C) 2018  Ricardo Birjukovs Canelas
    !
    !This program is free software: you can redistribute it and/or modify
    !it under the terms of the GNU General Public License as published by
    !the Free Software Foundation, either version 3 of the License, or
    !(at your option) any later version.
    !
    !This program is distributed in the hope that it will be useful,
    !but WITHOUT ANY WARRANTY; without even the implied warranty of
    !MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    !GNU General Public License for more details.
    !
    !You should have received a copy of the GNU General Public License
    !along with this program.  If not, see <https://www.gnu.org/licenses/>.


    program TestsList

    use list_mod
    use integer_list_mod
    use shape_array_mod
    use types_mod

    implicit none

    integer i
    type(list) :: simple_list
    type(integerList) :: int_list
    type(shapeArray)  :: shp_array
    integer values(10)
    type(shape) :: square
    type(circle) :: round
    integer :: test_spots

    square%id = 16
    square%filled = .true.

    !List
    do i=1, 10
        call simple_list%add(i)
    enddo
    call simple_list%addValue(square)
    call simple_list%add(1.23)
    call simple_list%add('A')
    call simple_list%add('B')
    call simple_list%add('C')

    square%id=-8

    print*, "From the list implementation"
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

    print*, "Recovering the values to a variable and printing the variable"
    print *, values

    !shapeArray Tests
    test_spots = 5
    call shp_array%init(test_spots)
    do i=1, test_spots
        square%id = i
        call shp_array%put(i, square)
    enddo

    print*, "From our amazing container array!"
    call shp_array%printArray()

    round%id = 517
    round%radius = 1.78
    round%filled = .false.

    call shp_array%put(1, round)
    print*, "Putting back a value on our amazing container array!"
    call shp_array%printArray()

    test_spots=3
    call shp_array%resize(test_spots)
    do i=6, test_spots
        square%id = i
        call shp_array%put(i, square)
    enddo

    print*, "From our amazing resized container array!"
    call shp_array%printArray()
    call shp_array%printElement(1)
    call shp_array%printElement(5)

    end program TestsList

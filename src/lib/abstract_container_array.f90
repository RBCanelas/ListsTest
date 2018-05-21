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
    
    !------------------------------------------------------------------------------
    !        IST/MARETEC, Water Modelling Group, Mohid modelling system
    !------------------------------------------------------------------------------
    !
    ! AFFILIATION   : IST/MARETEC, Marine Modelling Group
    ! DATE          : May 2018
    ! REVISION      : Canelas 0.2
    !> @author
    !> Ricardo Birjukovs Canelas
    !
    ! DESCRIPTION:
    !> Module that defines an unlimited polymorphic container class and related 
    !> methods. A container is a fundamental entity allowing to build data 
    !> structures such as lists and arrays.
    !> This is an abstract type, so a derived type must be defined for any 
    !> specific contents that may be required. Those derived types should provide 
    !> type-specific methods that require type-guards, such as printing. 
    !------------------------------------------------------------------------------

    module abstract_container_array_mod

    use container_mod

    private
    !Public access vars
    public :: container_array

    type, abstract :: container_array !< Abstract unlimited polymorphic array class
        private
        class(container), allocatable, dimension(:) :: contents !< Allocatable unlimited polymorphic container array 
        integer :: length !< Lenght of the array, for easy access
    contains
    procedure :: resize => resizeArray  !< Grows (adds empty space) or shrinks (discards the last entries) of the array
    procedure :: init => initArray !< Allocates the container array. Deallocates if already allocated
    procedure, non_overridable :: getValue !< returns the requested entry (pointer)
    procedure, non_overridable :: putValue !< stores a value on the requested index
    procedure, non_overridable :: getLength !< returns the length of the array
    generic :: put => putValue
    generic :: get => getValue
    end type container_array

    contains

    
    !---------------------------------------------------------------------------
    !> @Ricardo Birjukovs Canelas - MARETEC
    !> @brief
    !> Method that returns returns the requested entry (pointer)
    !> @param[this, index]
    !---------------------------------------------------------------------------
    function getValue(this, index)
    class(container_array) :: this
    integer :: index
    class(*), pointer :: getValue
    if (index .le. this%getLength()) then
        getValue => this%contents(index)%getContent()
    else
        stop '[getValue]: index out of bounds'
    endif
    end function getValue

    !---------------------------------------------------------------------------
    !> @Ricardo Birjukovs Canelas - MARETEC
    !> @brief
    !> Method that stores a value on the requested index
    !> @param[this, index, value]
    !---------------------------------------------------------------------------
    subroutine putValue(this, index, value)
    class(container_array) :: this
    integer :: index
    class(*) :: value
    if (index .le. this%getLength()) then
        call this%contents(index)%storeContent(value)
    else
        stop '[putValue]: index out of bounds'
    endif
    end subroutine putValue

    !---------------------------------------------------------------------------
    !> @Ricardo Birjukovs Canelas - MARETEC
    !> @brief
    !> Method that returns the length of the array
    !> @param[this]
    !---------------------------------------------------------------------------
    function getLength(this)
    class(container_array) :: this
    integer :: getLength
    getLength = this%length
    end function getLength

    !---------------------------------------------------------------------------
    !> @Ricardo Birjukovs Canelas - MARETEC
    !> @brief
    !> Method that grows (adds empty space) or shrinks (discards the 
    !> last entries) of the array
    !> @param[this, newsize]
    !---------------------------------------------------------------------------
    subroutine resizeArray(this,newsize)
    class(container_array) :: this
    integer :: newsize
    integer :: i, tocopy
    type(container), allocatable, dimension(:) :: temp
    tocopy=min(this%getLength(),newsize)
    allocate(temp(newsize))
    do i=1, tocopy
        call temp(i)%storeContent(this%getValue(i))
    enddo
    call this%init(newsize)
    do i=1, tocopy
        call this%putValue(i,temp(i)%getContent())
    enddo
    end subroutine resizeArray

    !---------------------------------------------------------------------------
    !> @Ricardo Birjukovs Canelas - MARETEC
    !> @brief
    !> Method that allocates the container array. Deallocates if already allocated
    !> @param[this, entries]
    !---------------------------------------------------------------------------
    subroutine initArray(this,entries)
    class(container_array) :: this
    integer :: entries
    if (allocated(this%contents)) then
        deallocate(this%contents)
    end if
    allocate(this%contents(entries))
    this%length=entries
    end subroutine initArray


    end module abstract_container_array_mod

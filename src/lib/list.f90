!
!     Copyright (c) 2015, NVIDIA CORPORATION.  All rights reserved.
!
! NVIDIA CORPORATION and its licensors retain all intellectual property
! and proprietary rights in and to this software, related documentation
! and any modifications thereto.  Any use, reproduction, disclosure or
! distribution of this software and related documentation without an express
! license agreement from NVIDIA CORPORATION is strictly prohibited.
!

!          THIS CODE AND INFORMATION ARE PROVIDED "AS IS" WITHOUT
!   WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT
!   NOT LIMITED TO THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR
!   FITNESS FOR A PARTICULAR PURPOSE.
!

!! list.f90

module list_mod
  use link_mod
  private
  public :: list
  type list
     private
     class(link),pointer :: firstLink => null() ! first link in list
     class(link),pointer :: lastLink => null()  ! last link in list
   contains
     procedure :: printValues ! print linked list
     procedure :: addInteger  ! add integer to linked list
     procedure :: addChar     ! add character to linked list
     procedure :: addReal     ! add real to linked list
     procedure :: addValue    ! add class(*) to linked list
     procedure :: firstValue  ! return value associated with firstLink
     procedure :: isEmpty     ! return true if list is empty
     generic :: add => addInteger, addChar, addReal
  end type list

contains

  subroutine printValues(this)
    class(list) :: this

    if (.not.this%isEmpty()) then
       call this%firstLink%printLinks()
    endif
  end subroutine printValues

  subroutine addValue(this, value)
    class(list) :: this
    class(*) :: value
    class(link), pointer :: newLink

    if (.not. associated(this%firstLink)) then
       this%firstLink => link(value, this%firstLink)
       this%lastLink => this%firstLink
    else
       newLink => link(value, this%lastLink%nextLink())
       call this%lastLink%setNextLink(newLink)
       this%lastLink => newLink
    end if

  end subroutine addValue

  subroutine addInteger(this, value) 
   class(list) :: this
    integer value
    class(*), allocatable :: v

    allocate(v,source=value)
    call this%addValue(v)
  end subroutine addInteger

  subroutine addChar(this, value)
    class(list) :: this
    character :: value
    class(*), allocatable :: v

    allocate(v,source=value)
    call this%addValue(v)
  end subroutine addChar

  subroutine addReal(this, value)
    class(list) :: this
    real value
    class(*), allocatable :: v

    allocate(v,source=value)
    call this%addValue(v)
  end subroutine addReal

  function firstValue(this)
    class(list) :: this
    class(*), pointer :: firstValue

    firstValue => this%firstLink%getValue()

  end function firstValue

  function isEmpty(this)
    class(list) :: this
    logical isEmpty

    if (associated(this%firstLink)) then
       isEmpty = .false.
    else
       isEmpty = .true.
    endif
  end function isEmpty

end module list_mod 
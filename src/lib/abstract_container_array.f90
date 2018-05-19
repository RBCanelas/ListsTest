
module abstract_container_array_mod

  use container_mod

  private
  public :: container_array

  type, abstract :: container_array
     private
     class(container), allocatable, dimension(:) :: contents
     integer :: length
   contains
     procedure :: resize => resizeArray
     procedure :: init => initArray
     procedure, non_overridable :: getValue
     procedure, non_overridable :: putValue
     procedure, non_overridable :: getLength
     generic :: put => putValue
     generic :: get => getValue
  end type container_array

contains

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

  function getLength(this)
    class(container_array) :: this
    integer :: getLength
    getLength = this%length
  end function getLength

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

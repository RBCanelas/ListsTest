
module abstract_container_array_mod

  use container_mod

  private
  public :: container_array

  type, abstract :: container_array
     private
     class(container), allocatable, dimension(:) :: contents
     integer :: length
   contains
     procedure, non_overridable :: getValue
     procedure, non_overridable :: addValue
     generic :: add => addValue
  end type container_array

contains

  subroutine addValue(this, index, value)
    class(container_array) :: this
    integer :: index
    class(*) :: value
    call this%contents(index)%storeValue(value)
  end subroutine addValue

  function getValue(this, index)
    class(container_array) :: this
    integer :: index
    class(*), pointer :: CurrentValue
    CurrentValue => this%contents(index)%getValue()
  end function getValue

  subroutine initArray(this,entries)
    class(container_array) :: this
    integer :: entries
    allocate(this%contents(entries))
    this%length=entries
  end subroutine initArray


end module abstract_container_array_mod

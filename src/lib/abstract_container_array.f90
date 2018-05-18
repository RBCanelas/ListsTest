
module abstract_container_array_mod

  use container_mod

  private
  public :: container_array

  type, abstract :: container_array
     !private
     class(container),allocatable, dimension(:) :: contents
     integer :: lenght
   contains
     procedure, non_overridable :: addValue
     generic :: add => addValue
  end type container_array

contains

  subroutine addValue(this, index, value)
    class(container_array) :: this
    integer :: index
    class(*) :: value

  end subroutine addValue


end module abstract_container_array_mod

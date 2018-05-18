module container_mod

  use types_mod

  private
  public :: container

  type container
    private
    class(*), pointer :: value => null() ! value stored in container
  contains
    procedure :: getValue    ! return value pointer
    procedure :: printContainer  ! print container contents
  end type container

  interface container
    procedure constructor ! construct/initialize a container
  end interface

contains

  function getValue(this)
    class(container) :: this
    class(*), pointer :: getValue
    getValue => this%value
  end function getValue

  subroutine printContainer(this)
    class(container) :: this

    select type(v => this%value)
    type is (integer)
      print *, v
    type is (character(*))
      print *, v(1:1)
    type is (real)
      print *, v
    type is (shape)
      call v%print()
      class default
      stop 'printLink: unexepected type for container content printing'
    end select

  end subroutine printContainer

  function constructor(value)
    class(container),pointer :: constructor
    class(*) :: value
    allocate(constructor)
    allocate(constructor%value, source=value)
  end function constructor

end module container_mod

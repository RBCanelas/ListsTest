module container_mod

  implicit none
  private
  public :: container

  type container
    private
    class(*), pointer :: value => null() ! value stored in container
  contains
    procedure :: getContent    ! return value pointer
    procedure :: storeContent  !put in the container
    procedure :: printContainer  ! print container contents
  end type container

  interface container
    procedure constructor ! construct/initialize a container
  end interface

contains

  function getContent(this)
    class(container) :: this
    class(*), pointer :: getContent
    getContent => this%value
  end function getContent

  subroutine storeContent(this,to_store)
    class(container) :: this
    class(*) :: to_store
    allocate(this%value, source=to_store)
  end subroutine storeContent

  subroutine printContainer(this)
    class(container) :: this
    select type(v => this%value)
    type is (integer)
      print *, v
    type is (character(*))
      print *, v(1:1)
    type is (real)
      print *, v
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

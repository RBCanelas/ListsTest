
module shape_array_mod

  use container_mod

  private
  public :: shapeArray

  type :: shapeArray
    private
    class(container), allocatable, dimension(:) :: contents
    integer :: length
  contains
    procedure :: initshapeArray
    procedure :: printArray => printshapeArray
    procedure, non_overridable :: addValue
    generic :: add => addValue
  end type shapeArray

  interface shapeArray
    procedure constructor ! construct/initialize
  end interface

contains

  !---------------------------------------------------
  subroutine printshapeArray(this)
    class(shapeArray) :: this
    class(*), pointer :: curr


  end subroutine printshapeArray

  subroutine addValue(this, index, value)
    class(shapeArray) :: this
    integer :: index
    class(*) :: value

  end subroutine addValue

  function constructor(entries)
    class(shapeArray), pointer :: constructor
    integer :: entries
    allocate(constructor)
    call constructor%initshapeArray(entries)
  end function constructor

  subroutine initshapeArray(this,entries)
    class(shapeArray) :: this
    integer :: entries
    allocate(this%contents(entries))
    this%length=entries
  end subroutine initshapeArray

end module shape_array_mod

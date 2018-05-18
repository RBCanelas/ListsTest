
module shape_array_mod

  use container_mod
  use types_mod

  private
  public :: shapeArray

  type :: shapeArray
    private
    class(container), allocatable, dimension(:) :: contents
    integer :: length
  contains
    procedure :: init => initshapeArray
    procedure :: printArray => printshapeArray
    procedure, non_overridable :: addValue
    generic :: add => addValue
  end type shapeArray

contains

  !---------------------------------------------------
  subroutine printshapeArray(this)
    class(shapeArray) :: this
    class(*), pointer :: curr
    integer :: i
    do i=1, this%length
        curr => this%contents(i)%getValue()
        select type(curr)
        type is (integer)
          print *, curr
        type is (character(*))
          print *, curr(1:1)
        type is (real)
          print *, curr
        type is (shape)
          call curr%print()
        class is (circle)
          call curr%print()
          class default
          stop 'printArray: unexepected type for container content printing'
        end select
    enddo
  end subroutine printshapeArray

  subroutine addValue(this, index, value)
    class(shapeArray) :: this
    integer :: index
    class(*) :: value
    call this%contents(index)%storeValue(value)
  end subroutine addValue

  subroutine initshapeArray(this,entries)
    class(shapeArray) :: this
    integer :: entries
    allocate(this%contents(entries))
    this%length=entries
  end subroutine initshapeArray

end module shape_array_mod

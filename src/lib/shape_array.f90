
module shape_array_mod

  use abstract_container_array_mod
  use types_mod

  private
  public :: shapeArray

  type, extends(container_array) :: shapeArray
  contains
    procedure :: printArray => printshapeArray
    procedure :: printElement => printshapeElement
  end type shapeArray

contains

  subroutine printshapeArray(this)
    class(shapeArray) :: this
    class(*), pointer :: curr
    integer :: i
    do i=1, this%getLength()
      curr => this%getValue(i)
      select type(curr)
      type is (shape)
        call curr%print()
      class is (circle)
        call curr%print()
        class default
        stop '[printshapeArray]: unexepected type of content: not a shape or derived type'
      end select
    end do
  end subroutine printshapeArray

  subroutine printshapeElement(this,index)
    class(shapeArray) :: this
    integer :: index
    class(*), pointer :: curr
    if (index .le. this%getLength()) then
      curr => this%getValue(index)
      select type(curr)
      type is (shape)
        call curr%print()
      class is (circle)
        call curr%print()
        class default
        stop '[printshapeElement]: unexepected type of content, not a shape or derived type'
      end select
    else
      stop '[printshapeElement]: index out of bounds'
    endif
  end subroutine printshapeElement

end module shape_array_mod

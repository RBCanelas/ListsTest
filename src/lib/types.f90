module types_mod

  private
  public :: shape, circle

  type shape
      integer :: id
      real :: posx, posy
      logical :: filled
   contains
     procedure :: print=>printShape  ! print shape
   end type shape
   
   type, extends(shape) :: circle
      integer :: radius
   contains
     procedure :: print=>printCircle  ! print shape
  end type circle

contains

  subroutine printShape(this)
    class(shape) :: this
    print*, 'id = ', this%id
    print*, 'is filled = ', this%filled
  end subroutine printShape
  
  subroutine printCircle(this)
    class(circle) :: this
    print*, 'id = ', this%id
    print*, 'radius = ', this%radius
    print*, 'is filled = ', this%filled
  end subroutine printCircle


end module types_mod

module types_mod

  private
  public :: shape

  type shape
      integer :: id
      real :: posx, posy
      logical :: filled
   contains
     procedure :: print=>printShape  ! print shape
  end type shape

contains

  subroutine printShape(this)
    class(shape) :: this
    print*, 'id = ', this%id
    print*, 'is filled = ', this%filled
  end subroutine printShape


end module types_mod

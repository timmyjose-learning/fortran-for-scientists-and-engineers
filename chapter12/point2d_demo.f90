program point2d_demo
   use, intrinsic :: iso_fortran_env, only: real64
   implicit none

   integer, parameter :: realk = real64

   type :: point_t
      real(kind=realk) :: x
      real(kind=realk) :: y
   end type point_t

   ! type constructor / structure constructor
   type(point_t) :: p = point_t(1.0_realk, -3.445_realk)

   call demo(p)

contains
   subroutine demo(pt)
      implicit none

      type(point_t), intent(in) :: pt

      write (*, '(A,F10.2,A,F10.2,A)') '(', pt%x, ', ', pt%y, ')'
   end subroutine demo
end program point2d_demo

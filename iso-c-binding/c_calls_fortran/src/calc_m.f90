module calc_m
   use, intrinsic :: iso_c_binding, only: c_float
   implicit none
   private

   public :: my_sub

contains
   subroutine my_sub(a, b, c) bind(C)
      implicit none

      real(kind=c_float) :: a, b, c

      c = a * b
   end subroutine my_sub
end module calc_m

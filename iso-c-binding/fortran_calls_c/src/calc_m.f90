module calc_m
   use, intrinsic :: iso_c_binding, only: c_float
   implicit none
   private

   interface
      subroutine calc(a, b, c) bind(c)
        import :: c_float
         implicit none

         real(kind=c_float), intent(in) :: a, b
         real(kind=c_float), intent(inout) :: c
      end subroutine calc
   end interface

   public :: calc
end module calc_m

! Note:
! For keyword arguments support, the procedure must have an explicit interface (either by declaring it in a module, via explicit
! interface declations, or whichever other method)
! Conventional dummy arguments and keyword arguments may be mixed and matched, but once a keyword argument is introduced, the
! remaining (to the right) arguments must also be keyword arguments (non-conventional arguments)

module calc_m
   implicit none
   private

   public :: calc

contains
   function calc (first, second, third)
      implicit none

      real, intent(in) :: first, second, third
      real :: calc

      calc = (first - second) / third
   end function calc
end module calc_m

program keyword_arguments_demo
   use calc_m, only: calc
   implicit none

   real :: a, b, c

   write (*, *) 'Enter three real numbers'
   read (*, *) a, b, c

   ! Call using conventional arguments
   write (*, *) calc(a, b, c)

   ! Call using keyword arguments only
   write (*, *) calc(third=c, first=a, second=b)
   write (*, *) calc(second=b, third=c, first=a)
   write (*, *) calc(first=a, second=b, third=c)

   ! Call using conventional arguments and keyword arguments
   write (*, *) calc(a, third=c, second=b)
   write (*, *) calc(a, b, third=c)
end program keyword_arguments_demo
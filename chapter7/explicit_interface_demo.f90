module subs_m
   implicit none

contains
   subroutine sub(n)
      implicit none

      integer, intent(in) :: n

      write (*, *) n
   end subroutine
end module subs_m

program explicit_interface_demo
   use subs_m
   implicit none

   integer :: x = 100
   real :: r = 2.7828

   call sub(x)
   ! Defining procedures inside modules provides better type checking due to explicit interfaces.
   ! This is now caught at compile time.
   !call sub(r)
end program explicit_interface_demo


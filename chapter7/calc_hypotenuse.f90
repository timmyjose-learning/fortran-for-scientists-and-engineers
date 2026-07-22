program calc_hypotenuse
   implicit none

   real :: a, b
   real :: hypo

   write (*, *) 'Enter the two sides of the right triangle'
   read (*, *) a, b

   call hypotenuse(a, b, hypo)
   write (*, '(A,F10.3)') 'Hypotenuse = ', hypo

contains
   subroutine hypotenuse(x, y, h)
      implicit none

      real, intent(in) :: x, y
      real, intent(out) :: h

      h = sqrt(x**2 + y**2)
   end subroutine hypotenuse
end program calc_hypotenuse

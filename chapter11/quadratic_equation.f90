! Demonstrating how the complex type can be used to solve for the roots of the quadratic equation
! in a much simpler way

program quadratic_equation
   implicit none

   integer, parameter :: DBL = selected_real_kind(p=13)
   real(kind=DBL) :: aa, bb, cc

   write (*, *) 'Enter the coefficients'
   read (*, *) aa, bb, cc

   call find_roots(aa, bb, cc)

contains
   subroutine find_roots(a, b, c)
      implicit none

      real(kind=DBL), intent(in) :: a, b, c
      real(kind=DBL) :: discriminant
      complex(kind=DBL) :: r1, r2

      discriminant = b**2 - 4.0 * a * c

      r1 = (-b + sqrt(cmplx(discriminant, 0.0, kind=DBL))) /  (2.0 * a)
      r2 = (-b - sqrt(cmplx(discriminant, 0.0, kind=DBL))) / (2.0 * a)

      write (*, *) 'Root 1 = ', r1
      write (*, *) 'Root 2 = ', r2
   end subroutine find_roots
end program quadratic_equation
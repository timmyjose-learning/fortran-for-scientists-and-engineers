program check_complex_roots
   implicit none

   real :: a, b, c
   real :: discriminant

   write (*, *) 'Enter a, b, c for the quadratic equation'
   read (*, *) a, b, c

   discriminant = b*b - 4.0*a*c

   if (discriminant < 0.0) then
      write (*, *) 'The quadratic equation has two complex roots'
   end if
end program check_complex_roots

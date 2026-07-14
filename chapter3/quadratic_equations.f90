program quadratic_equations
   implicit none

   real :: a, b, c, discriminant

   write (*, *) 'Enter a, b, c for the quadratic equation'
   read (*, *) a, b, c

   discriminant = b*b - 4.0*a*c

   if (discriminant < 0.0) then
      write (*, *) 'Complex roots'
   else if (discriminant > 0.0) then
      write (*, *) 'Real roots'
   else
      write (*, *) 'Identical real roots'
   end if
end program quadratic_equations

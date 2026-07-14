program roots
   implicit none

   real :: a, b, c, discriminant
   real :: root1, root2, real_part, imag_part

   write (*, *) 'Enter a, b, and c for the quadratic equation'
   read (*, *) a, b, c

   discriminant = b*b - 4.0*a*c

   if (discriminant < 0.0) then
      write (*, *) 'Complex roots'

      real_part = -b/(2.0*a)
      imag_part = sqrt(abs(discriminant))/(2.0*a)
      write (*, *) 'Root1 = ', real_part, '+i', imag_part
      write (*, *) 'Root2 = ', real_part, '-i', imag_part
   else if (discriminant > 0.0) then
      write (*, *) 'Distinct real roots'

      root1 = (-b + sqrt(discriminant))/(2.0*a)
      root2 = (-b - sqrt(discriminant))/(2.0*a)
      write (*, *) 'Root1 = ', root1
      write (*, *) 'Root2 = ', root2
   else
      write (*, *) 'Identical real roots'

      root1 = -b/(2.0*a)
      write (*, *) 'Root1 = Root2 = ', root1
   end if
end program roots

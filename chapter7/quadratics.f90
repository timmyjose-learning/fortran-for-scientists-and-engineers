program quadratics
   implicit none

   real :: a, b, c, x

   interface
      real function quadf(aa, bb, cc, xx)
         implicit none

         real, intent(in) :: aa, bb, cc, xx
      end function quadf
   end interface

   write (*, *) 'Enter the coefficients of the quadratic exprssion (a, b, c)'
   read (*, *) a, b, c

   write (*, *) 'Enter the value of x'
   read (*, *) x

   write (*, *) quadf(a, b, c, x)
end program quadratics

real function quadf(a, b, c, x)
   implicit none

   real, intent(in) :: a, b, c, x

   quadf = a*x**2 + b*x + c
end function quadf

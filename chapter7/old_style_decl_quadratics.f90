program old_style_decl_quadratics
   implicit none

   real :: quadf ! the function declaration
   real :: a, b, c, x

   write (*, *) 'Enter the coefficients (a, b, c)'
   read (*, *) a, b, c

   write (*, *) 'Enter x'
   read (*, *) x

   write (*, *) quadf(a, b, c, x)
end program old_style_decl_quadratics

real function quadf(a, b, c, x)
   implicit none

   real, intent(in) :: a, b, c, x

   quadf = a*x**2 + b*x + c
end function quadf

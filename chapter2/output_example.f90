program output_example
   implicit none

   integer :: idx
   real :: theta

   idx = 1
   theta = 3.141593

   write (*, *) 'idx = ', idx
   write (*, *) 'theta = ', theta
   write (*, *) 'cos(theta) = ', cos(theta)
   write (*, *) real(idx), nint(theta)
end program output_example

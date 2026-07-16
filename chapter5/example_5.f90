! rESw.d
! Ensure: w >= d + 7

program example_5
   implicit none

   real :: a = 1.2346E6, b = 0.001, c = -77.7E10

   write (*, '(2ES14.4, ES12.6)') a, b, c
end program example_5

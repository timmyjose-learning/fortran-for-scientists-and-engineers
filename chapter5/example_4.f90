! rEw.d
! r = repetition count, w = width, d = number of digits after the decimal point
! Ensure: w >= d + 7

program example_4
   implicit none

   real :: a = 1.2346E6, b = 0.001, c = -77.7E10, d = -77.7E10

   write (*, '(2E14.4, E13.6, E11.6)') a, b, c, d
end program example_4

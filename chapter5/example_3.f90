! rFw.d
! r = repetition count, w = width, d = number of digits after the decimal point
program example_3
   implicit none

   real :: a = -12.3, b = 0.123, c = 123.456

   write (*, '(2F6.3, F8.3)') a, b, c
   write (*, '(3F10.2)') a, b, c
end program example_3

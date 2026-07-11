program quiz_1
   implicit none

   integer :: i
   real :: a

   a = 0.05
   i = nint(2.0*3.141593/a)
   a = a*(5/3)
   write (*, *) i, a
end program quiz_1

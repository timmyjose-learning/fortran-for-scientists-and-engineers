program quiz_2
   implicit none

   integer :: i, j, k
   real :: a, b, c

   read (*, *) i, j, a
   read (*, *) b, k

   c = sin((3.141593/180)*a)
   write (*, *) i, j, k, a, b, c
end program quiz_2

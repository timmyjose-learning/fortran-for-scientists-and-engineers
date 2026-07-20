program basic_demo
   implicit none

   integer :: a(5)
   integer :: idx

   do idx = 1, 5
      a(idx) = idx*2 + 100
   end do

   write (*, *) a
end program basic_demo

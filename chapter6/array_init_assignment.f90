program array_init_assignment
   implicit none

   real, dimension(10) :: a
   integer :: idx

   do idx = 1, 10
      a(idx) = real(idx)
   end do

   write (*, *) a
end program array_init_assignment

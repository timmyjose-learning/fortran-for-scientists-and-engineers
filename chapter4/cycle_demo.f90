program cycle_demo
   implicit none

   integer :: i

   do i = 1, 5
      if (i == 3) then
         cycle
      end if

      write (*, *) i
   end do
end program cycle_demo

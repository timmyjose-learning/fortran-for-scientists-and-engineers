program exit_demo
   implicit none

   integer :: i

   do i = 1, 5
      if (i == 3) then
         exit
      end if

      write (*, *) i
   end do
end program exit_demo

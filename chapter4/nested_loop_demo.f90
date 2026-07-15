program nested_loop_demo
   implicit none

   integer :: i, j

   outer: do i = 1, 5
      inner: do j = 1, 5
         write (*, *) i*j
      end do inner
   end do outer
end program nested_loop_demo

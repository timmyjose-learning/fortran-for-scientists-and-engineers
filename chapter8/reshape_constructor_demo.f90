program reshape_construactor_demo
   implicit none

   integer, dimension(4, 3) :: arr
   integer :: i, j

   ! Fortran uses column-major ordering
   arr = reshape([1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3], [4, 3])

   do i = 1, size(arr, 1)
      write (*, '(*(I5))') (arr(i, j), j=1, size(arr, 2))
   end do
end program reshape_construactor_demo

program init_rank_2_array
   implicit none

   ! array constructors only work for rank 1 arrays, so we must use `reshape`
   integer, dimension(4, 3) :: a = reshape([1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3], [4, 3])
   integer :: i, j

   do i = 1, size(a, 1)
      write (*, '(*(I5))') (a(i, j), j=1, size(a, 2))
   end do
end program init_rank_2_array

program array_subscripts_rank_2_demo
   implicit none

   integer, dimension(5, 5) :: arr = reshape([1, 6, 11, 16, 21, 2, 7, 12, 17, 22, 3, 8, 13, 18, 23, 4, 9, 14, 19, 24, 5, 10, 15, 20, 25], [5, 5])

   call print_array_rank_2(arr)
   call print_array_rank_1(arr(:, 1))
   call print_array_rank_1(arr(1, :))
   call print_array_rank_2(arr(1:3, 1:5:2))

contains
   subroutine newline()
      implicit none

      write (*, *) ''
   end subroutine newline

   subroutine print_array_rank_2(a)
      implicit none

      integer, dimension(:, :), intent(in) :: a
      integer :: i, j

      do i = 1, size(a, 1)
         write (*, '(*(I5))') (a(i, j), j=1, size(a, 2))
      end do
      call newline()
   end subroutine print_array_rank_2

   subroutine print_array_rank_1(a)
      implicit none

      integer, dimension(:), intent(in) :: a
      integer :: i

      write (*, '(*(I5))') (a(i), i=1, size(a, 1))
      call newline()
   end subroutine print_array_rank_1
end program array_subscripts_rank_2_demo

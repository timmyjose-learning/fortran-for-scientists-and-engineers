! Again, use `do concurrent` instead of `forall`, which is now obsolete (since Fortran 2018)

program forall_demo_2
   implicit none

   real, dimension(2, 3) :: mat = reshape([-1, 4, -2, 5, 3, 6], [2, 3])
   real, dimension(2, 3) :: recip = 0.0

   call print_matrix(mat)
   write (*, *)
   call recip_matrix(mat, recip)
   call print_matrix(recip)

contains
   subroutine print_matrix(m)
      implicit none

      real, dimension(:, :), intent(in) :: m
      integer :: i, j

      do i = 1, size(m, 1)
         do j = 1, size(m, 2)
            write (*, '(*(F10.3))', advance='no') m(i, j)
         end do
         write (*, *)
      end do
   end subroutine print_matrix

   subroutine recip_matrix(m, r)
      implicit none

      real, dimension(:, :), intent(in) :: m
      real, dimension(:, :), intent(out) :: r
      integer :: i, j

      do concurrent(i=1:size(m, 1), j=1:size(m, 2), m(i, j) > 0.0)
         r(i, j) = 1.0/m(i, j)
      end do
   end subroutine recip_matrix
end program forall_demo_2

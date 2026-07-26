! NOTE: `forall` is obsolete. Use `do concurrent` instead.
program forall_demo_1
   implicit none

   real, dimension(10, 10) :: id_matrix = 0.0
   integer :: idx

   do concurrent(idx=1:10)
      id_matrix(idx, idx) = 1
   end do

   call print_matrix(id_matrix)

contains
   subroutine print_matrix(a)
      implicit none

      real, dimension(:, :), intent(in) :: a
      integer :: i, j

      do i = 1, size(a, 1)
         write (*, '(*(F10.3))') (a(i, j), j=1, size(a, 2))
      end do
   end subroutine print_matrix
end program forall_demo_1

program pass_expliciT_array_sizes
   implicit none

   integer, parameter :: ROW_SIZE = 4
   integer, parameter :: COL_SIZE = 5
   integer :: idx1, idx2
  integer, dimension(ROW_SIZE, COL_SIZE) :: arr = reshape([((idx1*idx2, idx1=1, ROW_SIZE), idx2=1, COL_SIZE)], [ROW_SIZE, COL_SIZE])
   integer, dimension(ROW_SIZE, COL_SIZE) :: dbl_arr

   call print_array(arr, ROW_SIZE, COL_SIZE)
   call double_array(arr, dbl_arr, ROW_SIZE, COL_SIZE)
   call print_array(dbl_arr, ROW_SIZE, COL_SIZE)

contains
   subroutine print_array(a, n, m)
      implicit none

      integer, intent(in) :: n, m
      integer, dimension(n, m), intent(in) :: a
      integer :: i, j

      do i = 1, n
         write (*, '(*(I5))') (a(i, j), j=1, m)
      end do
      write (*, *)
   end subroutine print_array

   subroutine double_array(a1, a2, n, m)
      implicit none

      integer, intent(in) :: n, m
      integer, dimension(n, m), intent(in) :: a1
      integer, dimension(n, m), intent(out) :: a2

      a2 = 2*a1
   end subroutine double_array
end program pass_explicit_array_sizes

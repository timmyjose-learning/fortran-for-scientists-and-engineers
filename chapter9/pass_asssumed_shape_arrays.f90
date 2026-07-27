program pass_assumed_shape_arrays
   implicit none

   integer, parameter :: ROW_SIZE = 4
   integer, parameter :: COL_SIZE = 5
   integer :: idx1, idx2
   integer, dimension(ROW_SIZE, COL_SIZE) :: arr = &
                                             reshape([((idx1*idx2, idx1=1, ROW_SIZE), idx2=1, COL_SIZE)], [ROW_SIZE, COL_SIZE])
   integer, dimension(ROW_SIZE, COL_SIZE) :: triple_arr

   call print_array(arr)
   call triple_array(arr, triple_arr)
   call print_array(triple_arr)

contains
   subroutine print_array(a)
      implicit none

      integer, dimension(:, :), intent(in) :: a
      integer :: i, j

      do i = 1, size(a, 1)
         write (*, '(*(I5))') (a(i, j), j=1, size(a, 2))
      end do
      write (*, *)
   end subroutine print_array

   subroutine triple_array(a1, a2)
      implicit none

      integer, dimension(:, :), intent(in) :: a1
      integer, dimension(:, :), intent(out) :: a2

      a2 = 3*a1
   end subroutine triple_array
end program pass_assumed_shape_arrays

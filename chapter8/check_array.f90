! Demonstrating inquiry intrinsics
program check_array
   implicit none

   real, dimension(-5:5, 0:3) :: a = 0.0

   call print_rank_2_array(a)
   call print_array_properties(a)

   write (*, *) 'Array properties when assumed-size is not used for array...'
   write (*, *) 'Size = ', size(a) ! 44
   write (*, *) 'Shape = ', shape(a) ! [11, 4]
   write (*, *) 'Lbound = ', lbound(a) ! [-5, 0]
   write (*, *) 'Ubound = ', ubound(a) ! [5, 3]

contains
   subroutine print_array_properties(arr)
      implicit none

      real, dimension(:, :), intent(in) :: arr

      write (*, *) 'Array shape = ', shape(arr) ! [11, 4]
      write (*, *) 'Size = ', size(arr) ! 44
      write (*, *) 'Lbround = ', lbound(arr) ! [1, 1]
      write (*, *) 'Ubound = ', ubound(arr) ! [11, 4]
   end subroutine print_array_properties

   subroutine print_rank_2_array(arr)
      implicit none

      real, dimension(:, :), intent(in) :: arr
      integer :: i, j

      do i = 1, size(arr, 1)
         write (*, '(*(F8.3))') (arr(i, j), j=1, size(arr, 2))
      end do
      write (*, *)
   end subroutine print_rank_2_array
end program check_array

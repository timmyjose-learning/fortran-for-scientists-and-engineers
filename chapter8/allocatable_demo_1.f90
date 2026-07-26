program allocatable_demo_1
   implicit none

   integer(kind=8), dimension(:, :), allocatable :: mat
   integer :: stat
   character(len=512) :: errmsg

   allocate (mat(2, 3), stat=stat, errmsg=errmsg)

   if (stat /= 0) then
      write (*, *) 'Failed to allocate memory: ', errmsg
      error stop
   end if

   mat = reshape([1, 4, 2, 5, 3, 6], [2, 3])
   call print_matrix(mat)

   if (allocated(mat)) then
      deallocate (mat, stat=stat)

      if (stat /= 0) then
         error stop 'Failed to deallocate memory'
      end if
   end if

contains
   subroutine print_matrix(m)
      implicit none

      integer(kind=8), dimension(:, :), intent(in) :: m
      integer :: i, j

      do i = 1, size(m, 1)
         write (*, '(*(I5))') (m(i, j), j=1, size(m, 2))
      end do
   end subroutine print_matrix
end program allocatable_demo_1

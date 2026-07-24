program read_rank_2_array_1
   implicit none

   character(len=512) :: filename
   integer :: iostat
   character(len=512) :: iomsg
   integer, dimension(4, 3) :: arr

   write (*, *) 'Enter the filename'
   read (*, *) filename

   call read_array(filename, arr, iostat, iomsg)

   if (iostat /= 0) then
      write (*, *) 'Error: ', iomsg
   else
      call print_array(arr)
   end if

contains
   subroutine print_array(a)
      implicit none

      integer, dimension(:, :), intent(in) :: a
      integer :: i, j

      do i = 1, size(a, 1)
         write (*, '(*(I5))') (a(i, j), j=1, size(a, 2))
      end do
   end subroutine print_array

   subroutine read_array(file, a, ios, iom)
      implicit none

      character(len=*), intent(in) :: file
      integer, dimension(:, :), intent(out) :: a
      integer, intent(out) :: ios
      character(len=*), intent(out) :: iom
      integer :: unit

      open (newunit=unit, file=file, status='old', action='read', iostat=ios, iomsg=iom)

      if (ios /= 0) then
         close (unit)
         return
      else
         read (unit, *) a
      end if

      close (unit)
   end subroutine read_array

end program read_rank_2_array_1

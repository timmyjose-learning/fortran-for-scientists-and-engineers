program selection_sort
   implicit none

   character(len=512) :: filename
   real, dimension(:), allocatable :: arr
   integer :: iostat
   character(len=512) :: iomsg

   write (*, *) 'Enter the filename of the file containing the numbers to be sorted'
   read (*, *) filename

   call read_numbers_from_file(filename, arr, iostat, iomsg)

   call print_array(arr)
   call ssort(arr)
   call print_array(arr)

contains
   subroutine read_numbers_from_file(file, nums, ios, iom)
      implicit none

      character(len=*), intent(in) :: file
      integer, intent(out) :: ios
      character(len=*), intent(out) :: iom
      real, dimension(:), allocatable, intent(out) :: nums
      integer :: unit
      real :: val
      integer :: i, nnums = 0

      ! first pass - read through the file and count the number of entries
      open (newunit=unit, file=file, status='old', action='read', iostat=ios, iomsg=iom)

      if (ios /= 0) then
         write (*, *) 'Error: cannot read from file, ', iom
         close (unit)
         error stop
      end if

      do
         read (unit, *, iostat=ios, iomsg=iom) val
         if (ios /= 0) exit
         nnums = nnums + 1
      end do

      if (ios > 0) then
         write (*, *) 'Error reading from file: ', iom
         close (unit)
         error stop
      end if

      allocate (nums(nnums))

      ! second pass, allocate the array and read into the array
      rewind (unit)

      do i = 1, nnums
         read (unit, *) nums(i)
      end do

      close (unit)
   end subroutine read_numbers_from_file

   subroutine print_array(nums)
      implicit none

      real, dimension(:), intent(in) :: nums
      integer :: i, len

      len = size(nums)

      write (*, '(*(G12.5,1X))') (nums(i), i=1, len)
   end subroutine print_array

   ! O(n^2) / O(1)
   subroutine ssort(nums)
      implicit none

      real, dimension(:), intent(inout) :: nums
      real :: temp = 0.0
      integer :: len = 0, i = 0, j = 0, min_idx = 0

      len = size(nums)

      do i = 1, len - 1
         min_idx = i
         do j = i + 1, len
            if (nums(j) < nums(min_idx)) then
               min_idx = j
            end if
         end do

         if (min_idx /= i) then
            temp = nums(i)
            nums(i) = nums(min_idx)
            nums(min_idx) = temp
         end if
      end do
   end subroutine ssort
end program selection_sort

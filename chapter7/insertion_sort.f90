program insertion_sort
   implicit none

   character(len=512) :: filename
   real, dimension(:), allocatable :: a
   integer :: iostat
   character(len=512) :: iomsg

   write (*, *) 'Enter the filename'
   read (*, *) filename

   call read_array(filename, a, iostat, iomsg)

   call print_array(a)
   call sort(a)
   call print_array(a)

   if (allocated(a)) deallocate (a)

contains
   subroutine read_array(file, vals, stat, msg)
      implicit none

      character(len=512), intent(in) :: file
      real, dimension(:), allocatable, intent(out) :: vals
      integer, intent(out) :: stat
      character(len=*), intent(out) :: msg
      real :: val
      integer :: i, nvals = 0
      integer :: unit

      open (newunit=unit, file=file, iostat=stat, iomsg=msg)

      if (stat /= 0) then
         write (*, *) 'Error: ', msg
         close (unit)
         error stop
      end if

      do
         read (unit, *, iostat=stat, iomsg=msg) val
         if (stat < 0) exit
         nvals = nvals + 1
      end do

      if (stat > 0) then
         write (*, *) 'Error: ', msg
         error stop
      end if

      allocate (vals(nvals))

      rewind (unit)

      do i = 1, nvals
         read (unit, *) vals(i)
      end do

      close (unit)
   end subroutine read_array

   subroutine print_array(vals)
      implicit none
      real, dimension(:), intent(in) :: vals
      integer :: i

      write (*, '(*(F8.3))') (vals(i), i=1, size(vals))
   end subroutine print_array

   subroutine sort(vals)
      implicit none

      real, dimension(:), intent(inout) :: vals
      integer :: i, j, len
      real :: key

      len = size(vals, 1)

      do i = 2, len
         key = vals(i)
         j = i - 1
         do while (j >= 1)
         if (vals(j) > key) then
            vals(j + 1) = vals(j)
            j = j - 1
         else
            exit
         end if
         end do
         vals(j + 1) = key
      end do
   end subroutine sort
end program insertion_sort

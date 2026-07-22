program add_nums
   implicit none

   integer :: a, b
   integer :: res
   integer :: iostat
   character(len=256) :: iomsg

   call read_num(a, iostat, iomsg)
   call read_num(b, iostat, iomsg)

   res = a + b
   write (*, '(A,I5,A,I5,A,I5)') 'The sum of ', a, ' and ', b, ' is ', res

contains
   subroutine read_num(x, ios, iom)
      implicit none

      integer, intent(out) :: x
      integer, intent(out) :: ios
      character(len=*), intent(out) :: iom

      write (*, *) 'Enter integer'
      read (*, *, iostat=ios, iomsg=iom) x

      if (ios /= 0) then
         write (*, *) 'Error: ', iom
         error stop
      end if
   end subroutine read_num
end program add_nums

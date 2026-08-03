program add_nums_cli
   implicit none

   call run_app()

contains
   subroutine run_app()
      implicit none

      integer :: x, y
      character(len=64) :: arg
      integer :: status
      integer :: iostat
      character(len=512) :: iomsg

      if (command_argument_count() /= 2) then
         error stop 'Need 2 numbers'
      end if

      call get_command_argument(1, arg, status=status)
      if (status /= 0) then
         error stop 'failed to read command line arg'
      end if

      x = parse_int(arg, iostat, iomsg)

      if (iostat /= 0) then
         write (*, *) iomsg
         return
      end if

      call get_command_argument(2, arg, status=status)

      if (status /= 0) then
         error stop 'failed to read command line arg'
      end if

      y = parse_int(arg, iostat, iomsg)
      if (iostat /= 0) then
         write (*, *) iomsg
         return
      end if

      write (*, *) 'Sum of ', x, ', and ', y, ' is ', x + y
   end subroutine run_app

   function parse_int(buffer, iostat, iomsg) result(num)
      implicit none

      character(len=*), intent(in) :: buffer
      integer :: num
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      read (buffer, *, iostat=iostat, iomsg=iomsg) num
   end function parse_int
end program add_nums_cli
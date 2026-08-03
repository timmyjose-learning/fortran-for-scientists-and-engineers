program command_line_args_demo
   implicit none

   character(len=64) :: command
   integer :: i
   integer :: status

   ! the program name will always be present
   call get_command_argument(0, command)
   write (*, *) 'Program name: ', trim(command)

   ! print all the command line arguments, if available
   do i = 1, command_argument_count()
      call get_command_argument(i, command, status=status)
      if (status /= 0) then
         error stop 'Error while retrieving command line argument'
      end if

      write (*, *) trim(command)
   end do
end program command_line_args_demo
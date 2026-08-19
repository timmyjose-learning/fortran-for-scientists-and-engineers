! This program leaks memory - most likely due to bugs in gfortran's asynchronous I/O implementation
program async_io_demo
   use, intrinsic :: iso_fortran_env, only: output_unit
   implicit none

   call run_app()

contains
   subroutine run_app()
      implicit none

      integer :: iostat
      character(len=512) :: iomsg

      call write_async(iostat, iomsg)
      call read_async(iostat, iomsg)
   end subroutine run_app

   subroutine write_async(iostat, iomsg)
      implicit none

      integer, intent(inout) :: iostat
      character(len=*), intent(inout) :: iomsg
      integer :: id
      character(len=:), allocatable :: message
      integer :: unit

      open (newunit=unit, file='async.dat', status='replace', action='write', asynchronous='yes', iostat=iostat, iomsg=iomsg)
      call check_stat(iostat, iomsg)

      id = 5
      message = 'Hello, asynchronous Fortran!'

      write (unit, '(I5,1X,A)', asynchronous='yes', iostat=iostat, iomsg=iomsg) id, message
      call check_stat(iostat, iomsg)

      wait(unit, iostat=iostat, iomsg=iomsg)
      call check_stat(iostat, iomsg)

      close (unit=unit, iostat=iostat, iomsg=iomsg)
      call check_stat(iostat, iomsg)
   end subroutine write_async

   subroutine read_async(iostat, iomsg)
      implicit none

      integer, intent(inout) :: iostat
      character(len=*), intent(inout) :: iomsg

      integer :: id
      character(len=512) :: message
      integer :: unit

      open (newunit=unit, file='async.dat', status='old', action='read', asynchronous='yes', iostat=iostat, iomsg=iomsg)
      call check_stat(iostat, iomsg)

      read (unit, '(I5,A)', asynchronous='yes', iostat=iostat, iomsg=iomsg) id, message
      call check_stat(iostat, iomsg)

      wait (unit, iostat=iostat, iomsg=iomsg)
      call check_stat(iostat, iomsg)

      write (output_unit, '("Id = ",I0,", message = ",A)') id, trim(adjustl(message))

      close (unit, iostat=iostat, iomsg=iomsg)
      call check_stat(iostat, iomsg)
   end subroutine read_async

   subroutine check_stat(iostat, iomsg)
      implicit none

      integer, intent(in) :: iostat
      character(len=*), intent(in) :: iomsg

      if (iostat /= 0) then
         error stop 'Error: ' // iomsg
      end if
   end subroutine check_stat
end program async_io_demo

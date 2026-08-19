program stream_copy
   use, intrinsic :: iso_c_binding, only: c_int8_t
   use, intrinsic :: iso_fortran_env, only: input_unit, output_unit, iostat_end
   implicit none

   call run_app()

contains
   subroutine run_app()
      implicit none

      character(len=:), allocatable :: filename
      character(len=512) :: temp_filename
      integer :: file_size
      integer :: f_in
      integer :: iostat
      character(len=512) :: iomsg

      character(len=:), allocatable :: outfile
      integer :: f_out

      integer, parameter :: BUF_SIZE = 8192
      integer(kind=c_int8_t), dimension(BUF_SIZE) :: buffer
      integer :: bytes_remaining
      integer :: chunk_to_read

      write (output_unit, *) 'Enter the source file'
      read (input_unit, *) temp_filename

      filename = trim(adjustl(temp_filename))
      outfile = filename // '_copy'

      inquire (file=filename, size=file_size, iostat=iostat, iomsg=iomsg)
      call check_stat(iostat, iomsg)

      if (file_size <= 0) then
         error stop 'input file is invalid or empty'
      end if

      open (newunit=f_in, file=filename, status='old', action='read', access='stream', form='unformatted', iostat=iostat, &
         iomsg=iomsg)
      call check_stat(iostat, iomsg)

      open (newunit=f_out, file=outfile, status='replace', action='write', access='stream', form='unformatted', iostat=iostat, &
         iomsg=iomsg)
      call check_stat(iostat, iomsg)

      bytes_remaining = file_size
      do while (bytes_remaining > 0)
         chunk_to_read = min(bytes_remaining, BUF_SIZE)

         read (unit=f_in, iostat=iostat, iomsg=iomsg) buffer(1:chunk_to_read)
         call check_stat(iostat, iomsg)

         write (unit=f_out, iostat=iostat, iomsg=iomsg) buffer(1:chunk_to_read)
         call check_stat(iostat, iomsg)

         bytes_remaining = bytes_remaining - chunk_to_read
      end do

      close (unit=f_in, iostat=iostat, iomsg=iomsg)
      call check_stat(iostat, iomsg)

      close (unit=f_out, iostat=iostat, iomsg=iomsg)
      call check_stat(iostat, iomsg)
   end subroutine run_app

   subroutine check_stat(iostat, iomsg)
      implicit none

      integer, intent(in) :: iostat
      character(len=*), intent(in) :: iomsg

      if (iostat /= 0) then
         error stop 'Error: ' // iomsg
      end if
   end subroutine check_stat
end program stream_copy
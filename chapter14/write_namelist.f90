program write_namelist
   use, intrinsic :: iso_fortran_env, only: error_unit
   implicit none

   call run_app()

contains
   subroutine run_app()
      implicit none

      integer :: i, j
      logical :: l
      character(len=21) :: string = 'This is a test string'
      integer :: unit
      integer :: iostat
      character(len=512) :: iomsg
      namelist / mylist / i, j, l, string

      i = 12
      j = -999
      l = .false.

      open(newunit=unit, file='output.nml', status='replace', action='write', iostat=iostat, iomsg=iomsg)
      if (iostat /= 0) then
         error stop 'Failed to open file for write: ' // iomsg
      end if

      write (unit=unit, nml=mylist, delim='apostrophe', iostat=iostat, iomsg=iomsg)
      if (iostat /= 0) then
         write (error_unit, *) 'Error while writing to file: ' // iomsg
      end if

      close(unit=unit, iostat=iostat, iomsg=iomsg)
      if (iostat /= 0) then
         write (error_unit, *) 'Error while closing file: ' // iomsg
      end if
   end subroutine run_app
end program write_namelist
program read_namelist
   use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
   implicit none

   call run_app()

contains
   subroutine run_app()
      implicit none

      integer :: i, j
      logical :: l
      character(len=21) :: string
      integer :: unit
      integer :: iostat
      character(len=512) :: iomsg
      namelist / mylist / i, j, l, string

      open(newunit=unit, file='output.nml', status='old', action='read', iostat=iostat, iomsg=iomsg)
      call check_stat(iostat, iomsg)

      read (unit=unit, nml=mylist, iostat=iostat, iomsg=iomsg)
      call check_stat(iostat, iomsg)

      write (output_unit, '("i = ",I0, ", j = ",I0, ", l = ",L1,", string = ",A)') i, j, l, string
      close(unit=unit, iostat=iostat, iomsg=iomsg)
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
end program read_namelist
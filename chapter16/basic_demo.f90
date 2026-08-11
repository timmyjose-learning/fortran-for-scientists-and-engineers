module complex_m
   use, intrinsic :: iso_fortran_env, only: error_unit
   implicit none
   private

   type :: complex_t
      private
      real :: re
      real :: im

   contains
      procedure, pass(this) :: print => print_complex
      generic :: write(formatted) => print
   end type complex_t

   public:: complex_t, make_complex

contains
   function make_complex(re, im) result(complex)
      implicit none

      real, intent(in) :: re, im
      type(complex_t), pointer :: complex
      integer :: stat
      character(len=512) :: errmsg

      allocate(complex, stat=stat, errmsg=errmsg)
      if (stat /= 0) then
         write (error_unit, *) 'Alloction failed: ' // errmsg
         error stop
      end if

      complex%re = re
      complex%im = im
   end function make_complex

   subroutine print_complex(this, unit, iotype, vlist, iostat, iomsg)
      implicit none

      class(complex_t), intent(in) :: this
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, dimension(:), intent(in) :: vlist
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      if (.false.) then
         write (unit, *) iotype
         write (unit, *) vlist
      end if

      write (unit, '("(",F8.3,", ",F8.3,")")', iostat=iostat, iomsg=iomsg) this%re, this%im
   end subroutine print_complex

end module complex_m

program basic_demo
   use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
   use complex_m, only: complex_t, make_complex
   implicit none

   call run_app()

contains
   subroutine run_app()
      implicit none

      ! polymorphic declarations must always be allocatable or pointer
      class(complex_t), pointer :: p
      integer :: stat
      character(len=512) :: errmsg

      p => make_complex(1.0, -2.0)
      write (output_unit, *) p

      if (associated(p)) then
         deallocate(p, stat=stat, errmsg=errmsg)
         if (stat /= 0) then
            write (error_unit, *) 'Deallocation failed: ' // errmsg
         end if
      end if
   end subroutine run_app
end program basic_demo
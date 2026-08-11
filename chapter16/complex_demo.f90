module complex_m
   implicit none
   private

   type :: complex_t
      private
      real :: re = 0.0
      real :: im = 0.0
   contains
      private
      procedure, pass(this) :: acc => add_complex_to_complex
      procedure, pass(this) :: arc => add_real_to_complex
      generic, public :: add => acc, arc

      procedure, pass(this) :: print => print_complex
      generic :: write(formatted) => print
   end type complex_t

   public :: complex_t, make_complex

contains
   function make_complex(re, im) result(complex)
      implicit none

      real, intent(in) :: re, im
      type(complex_t) :: complex

      complex%re = re
      complex%im = im
   end function make_complex

   function add_complex_to_complex(this, that) result(sum)
      implicit none

      class(complex_t), intent(in) :: this, that
      type(complex_t) :: sum

      sum%re = this%re + that%re
      sum%im = this%im + that%im
   end function add_complex_to_complex

   function add_real_to_complex(this, re) result(sum)
      implicit none

      class(complex_t), intent(in) :: this
      real, intent(in) :: re
      type(complex_t) :: sum

      sum%re = this%re + re
      sum%im = this%im + re
   end function add_real_to_complex

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

program complex_demo
   use, intrinsic :: iso_fortran_env, only: input_unit, output_unit
   use complex_m, only: complex_t, make_complex
   implicit none

   type(complex_t) :: c1, c2
   type(complex_t) :: res
   real :: re, im

   write (output_unit, *) 'Enter the first complex number'
   read (input_unit, *) re, im
   c1 = make_complex(re, im)

   write (output_unit, *) 'Enter the second complex number'
   read (input_unit, *) re, im
   c2 = make_complex(re, im)

   res = c1%add(c2)
   write (output_unit, *) 'c1 + c2 = ', res

   res = c1%add(1.0)
   write (output_unit, *) 'c1 + 1.0 = ', res
end program complex_demo
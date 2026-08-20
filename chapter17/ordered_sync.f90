program ordered_sync
   use, intrinsic :: iso_c_binding, only: c_int
   use, intrinsic :: iso_fortran_env, only: input_unit, output_unit
   implicit none

   interface
      function c_sleep(time_in_secs) bind(C, name='sleep') result(remaining)
         import :: c_int
         implicit none

         integer(kind=c_int), intent(in), value :: time_in_secs
         integer(kind=c_int) :: remaining
      end function c_sleep
   end interface

   call run_app()

contains
   subroutine run_app()
      implicit none

      integer, codimension[*], save :: a
      integer :: i
      integer :: m
      integer(kind=c_int) :: remaining

      if (this_image() == 1) then
         write (output_unit, *) 'Enter a number'
         read (input_unit, *) m

         do i = 1, num_images()
            a[i] = i * m
         end do
      end if

      ! order the output
      if (this_image() == 1) then
         remaining = c_sleep(2_c_int)
      end if

      if (this_image() > 1) sync images (this_image() - 1)
      write (output_unit, '(A,I0,A,I0)') 'Output from image ', this_image(), ' is ', a
      if (this_image() < num_images()) sync images (this_image() + 1)
   end subroutine run_app
end program ordered_sync
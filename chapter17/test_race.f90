program test_race
   use, intrinsic :: iso_fortran_env, only: output_unit
   implicit none

   call run_app()

contains
   subroutine run_app()
      implicit none

      integer, codimension[*], save :: num = 0
      integer :: i

      if (this_image() > 1) then
         sync images (this_image() - 1)
      end if

      if (this_image() == 1)  then
         do i = 1, num_images()
            num[i] = 0
         end do
      end if

      critical
         num[1] = num[1] + this_image()
         write (output_unit, '("num = ",I0," after image ",I0)') num[1], this_image()
      end critical

      if (this_image() < num_images()) then
         sync images (this_image() + 1)
      end if
   end subroutine run_app
end program test_race
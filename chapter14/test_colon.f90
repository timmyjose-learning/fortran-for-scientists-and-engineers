program test_colon
   use, intrinsic :: iso_fortran_env, only: output_unit
   implicit none

   call run_app()

contains
   subroutine run_app()
      implicit none

      real, dimension(8) :: arr
      integer :: i

      arr = (/ 1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8 /)

      write (output_unit, *) 'Without :'
      write (output_unit, '(3(5X,"X(",I2,") =",F8.3))') (i, arr(i), i = 1, size(arr))

      write (output_unit, *) 'With :'
      write (output_unit, '(3(:,5X, "X(",I2,") =",F8.3))') (i, arr(i), i = 1, size(arr))
   end subroutine run_app
end program test_colon
program test_read
   use, intrinsic :: iso_fortran_env, only: input_unit, output_unit
   implicit none

   call run_app()

contains
   subroutine run_app
      implicit none

      integer :: i, j, k

      i = 1
      j = 2
      k = 3

      write (output_unit, *) 'Enter the values of i, j, and k'
      read (input_unit, *) i, j, k
      write (output_unit, '("i = ",I0,", j = ",I0, ", k = ",I0)') i, j, k
   end subroutine run_app
end program test_read
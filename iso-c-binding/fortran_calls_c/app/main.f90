program main
   use, intrinsic :: iso_fortran_env, only: input_unit, output_unit
   use, intrinsic :: iso_c_binding, only: c_float
   use  calc_m, only: calc
   implicit none

   call run_app()

contains
   subroutine run_app()
      implicit none

      real(kind=c_float) a, b, c

      read (input_unit, *) a, b
      call calc(a, b, c)

      write (output_unit, '(F8.3," + ",F8.3," = ",F8.3)') a, b, c
   end subroutine run_app
end program main

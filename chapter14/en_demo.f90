! EN - engineering notation where the mantissa ia between 1 and 1000, and the exponent 10 ^ (multiple of 3)
program en_demo
   use, intrinsic :: iso_fortran_env, only: output_unit
   implicit none

   call run_app()

contains
   subroutine run_app()
      implicit none

      real :: a, b, c

      a = 1.2346E7
      b = 0.0001
      c = -77.7E10

      write (output_unit, '(3EN15.4)') a, b, c
   end subroutine run_app
end program en_demo
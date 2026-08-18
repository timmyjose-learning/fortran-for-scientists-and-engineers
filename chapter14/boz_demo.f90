program boz_demo
   use, intrinsic :: iso_fortran_env, only: output_unit
   implicit none

   call run_app()

contains
   subroutine run_app()
      implicit none

      integer :: a, b

      a = 16
      b = -1

      write (output_unit, '("Binary: ",B16,1X,B0)') a, b
      write (output_unit, '("Octal: ",O11.4,1X,O11.4)') a, b
      write (output_unit, '("Hex: ",Z8,1X,Z8)') a, b
   end subroutine run_app
end program boz_demo

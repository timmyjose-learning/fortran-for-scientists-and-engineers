program test_ptr
   use, intrinsic :: iso_fortran_env, only: output_unit
   implicit none

   real, pointer :: p => null()
   real, target :: t1 = 10.0, t2 = -17.0

   p => t1
   write (output_unit, *) 'p, t1, t2 = ', p, t1, t2

   p => t2
   write (output_unit, *) 'p, t1, t2 = ', p, t1, t2
end program test_ptr

program test_ptr2
   use, intrinsic :: iso_fortran_env, only: output_unit
   implicit none

   real, pointer :: p1 => null(), p2 => null()
   real, target :: t1 = 10.0, t2 = -17.0

   p1 => t1
   p2 => p1

   write (output_unit, '(A,4(F8.3))') 'p1, p2, t1, t2 = ', p1, p2, t1, t2

   p1 => t2
   write (output_unit, '(A,4(F8.3))') 'p1, p2, t1, t2 = ', p1, p2, t1, t2
end program test_ptr2
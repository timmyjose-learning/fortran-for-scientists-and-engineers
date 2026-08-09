program test_ptr3
   use, intrinsic :: iso_fortran_env, only: output_unit
   implicit none

   real, pointer :: p1 => null(), p2 => null(), p3 => null()
   real, target :: a = 11.0, b = 25.0, c = 22.0

   write (output_unit, *) associated(p1)

   p1 => a
   p2 => b
   p3 => c

   write (output_unit, *) associated(p1)
   write (output_unit, *) associated(p1, b)
   write (output_unit, *) associated(p1, target=c)
end program test_ptr3
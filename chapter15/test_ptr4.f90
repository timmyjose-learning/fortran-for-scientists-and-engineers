program test_ptr4
   use, intrinsic :: iso_fortran_env, only: output_unit
   implicit none

   real, pointer :: p1 => null(), p2 => null(), p3 => null()
   real, target :: a = 11.0, b = 12.5, c

   p1 => a ! p1 points to `a`
   p2 => b ! p2 points to `b`
   p3 => c ! p3 points to `c`

   ! Fortran auto-derereferences pointers in variable contexts
   p3 = p1 + p2 ! c = a + b (23.5)
   write (output_unit, *) 'p3 = ', p3

   p2 => p1 ! p2 now points to `a`
   p3 = p1 + p2 ! c = a + a (22.0)
   write (output_unit, *) 'p3 = ', p3

   p3 = p1 ! c = a
   p3 => p1 ! c = a (11.0)
   write (output_unit, *) 'p3 = ', p3
   write (output_unit, *) 'a, b, c = ', a, b, c ! 11.0, 12.5, 11.0
end program test_ptr4
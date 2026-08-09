program swap_ints_using_pointers
   use, intrinsic :: iso_fortran_env, only: output_unit
   implicit none

   integer, target :: a = 3, b = 5
   integer, pointer :: p1 => null(), p2 => null()

   p1 => a
   p2 => b

   write (output_unit, '("Before swapping, a = ",I5,", b = ",I5)') p1, p2
   call swap_ints(p1, p2)
   write (output_unit, '("After swapping, a = ",I5,", b = ",I5)') p1, p2

contains
   subroutine swap_ints(ptr1, ptr2)
      implicit none

      integer, pointer, intent(inout) :: ptr1, ptr2
      integer, pointer :: temp => null()

      temp => ptr1
      ptr1 = ptr2
      ptr2 = temp
   end subroutine swap_ints
end program swap_ints_using_pointers
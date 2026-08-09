program array_ptr
   use, intrinsic :: iso_fortran_env, only: output_unit
   implicit none

   integer :: i
   integer, dimension(16), target :: arr = [(i, i = 1, 16)]
   integer, dimension(:), pointer :: p1 , p2, p3, p4, p5

   p1 => arr
   call print_array(p1)

   p2 => p1(2::2) ! array section with indices: 2, 4, 6, 8, ..., 16
   call print_array(p2)

   p3 => p2(2::2) ! 4, 8, 12, 16
   call print_array(p3)

   p4 => p3(2::2) ! 8, 16
   call print_array(p4)

   p5 => p4(2::2) ! 16
   call print_array(p5)

contains
   subroutine print_array(a)
      implicit none

      integer, dimension(:), intent(in) :: a
      integer :: idx

      write (output_unit, '(*(I5))') (a(idx), idx = 1, size(a))
   end subroutine print_array
end program array_ptr
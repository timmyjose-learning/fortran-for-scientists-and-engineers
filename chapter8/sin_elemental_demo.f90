program sin_elemental_demo
   implicit none

   real, dimension(5) :: arr = [0.0, 3.141593, 1.0, 2.0, 3.0], res1, res2

   call apply_sin_one_by_one(arr, res1)
   call print_array(res1)

   call apply_in_bulk(arr, res2)
   call print_array(res2)

contains
   subroutine apply_in_bulk(a, b)
      implicit none

      real, dimension(:), intent(in) :: a
      real, dimension(:), intent(out) :: b

      b = sin(a)
   end subroutine apply_in_bulk

   subroutine apply_sin_one_by_one(a, b)
      implicit none

      real, dimension(:), intent(in) :: a
      real, dimension(:), intent(out) :: b
      integer :: i

      if (size(a, 1) /= size(b, 1)) then
         error stop 'array size mismatch'
      end if

      do i = 1, size(a, 1)
         b(i) = sin(a(i))
      end do
   end subroutine apply_sin_one_by_one

   subroutine print_array(a)
      implicit none

      real, dimension(:), intent(in) :: a
      integer :: i

      write (*, '(*(F8.3))') (a(i), i=1, size(a, 1))
   end subroutine print_array
end program sin_elemental_demo

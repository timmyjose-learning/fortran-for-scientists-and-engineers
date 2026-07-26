program example_8_3
   implicit none

   real, dimension(10) :: arr = [-1100, 1001, 1000, 999, -1200, -999, -50, 0, 12345, 1000]
   real, dimension(10) :: do_arr, where_arr

   call print_array(arr)

   call adjust_do(arr, do_arr)
   call print_array(do_arr)

   call adjust_where(arr, where_arr)
   call print_array(where_arr)

contains
   subroutine print_array(a)
      implicit none

      real, dimension(:), intent(in) :: a
      integer :: i

      write (*, '(*(F10.3,2X))') (a(i), i=1, size(a))
   end subroutine print_array

   subroutine adjust_do(a, r)
      implicit none

      real, dimension(:), intent(in) :: a
      real, dimension(:), intent(out) :: r
      integer :: i

      do i = 1, size(a)
         if (a(i) < -1000) then
            r(i) = -1000
         else if (a(i) > 1000) then
            r(i) = 1000
         else
            r(i) = a(i)
         end if
      end do
   end subroutine adjust_do

   subroutine adjust_where(a, r)
      implicit none

      real, dimension(:), intent(in) :: a
      real, dimension(:), intent(out) :: r

      where (a < -1000)
         r = -1000
      elsewhere(a > 1000)
         r = 1000
      elsewhere
         r = a
      end where
   end subroutine adjust_where
end program example_8_3

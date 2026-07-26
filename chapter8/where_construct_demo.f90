program where_construct_demo
   implicit none

   real, dimension(5) :: a = [-9.0, 1.1, 2.9, 0.0, 11.2]
   real, dimension(5) :: r

   call calculate_log(a, r)
   call print_array(r)

contains
   subroutine print_array(arr)
      implicit none

      real, dimension(:), intent(in) :: arr
      integer :: i

      write (*, '(*(F10.3,2X))') (arr(i), i=1, size(arr))
   end subroutine print_array

   subroutine calculate_log(arr, res)
      implicit none

      real, dimension(:), intent(in) :: arr
      real, dimension(:), intent(out) :: res

      if (size(arr) /= size(res)) then
         error stop 'array size mismatch'
      end if

      where (arr > 0)
         res = log(arr)
      elsewhere
         res = -99999
      end where
   end subroutine calculate_log
end program where_construct_demo

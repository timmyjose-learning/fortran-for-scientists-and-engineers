program explicit_shape_arrays
   implicit none

   integer, parameter :: ARR_SIZE = 5
   integer :: i
   real, dimension(ARR_SIZE) :: a = [1.0, 2.0, 3.0, 4.0, 5.0], b = [(i, i=1, ARR_SIZE)]

   call print_array(a, ARR_SIZE)
   call print_array(b, ARR_SIZE)

   call process(a, b, ARR_SIZE, ARR_SIZE - 2)

   call print_array(a, ARR_SIZE)
   call print_array(b, ARR_SIZE)

contains
   subroutine print_array(data, n)
      implicit none

      integer, intent(in) :: n
      real, dimension(n), intent(in) :: data
      integer :: j

      write (*, '(*(F8.3))') (data(j), j=1, n)
   end subroutine print_array

   subroutine process(data1, data2, n, nvals)
      implicit none

      integer, intent(in) :: n, nvals
      real, dimension(n), intent(in) :: data1
      real, dimension(n), intent(out) :: data2
      integer :: j

      do j = 1, nvals
         data2(j) = data1(j)*3.0
      end do
   end subroutine process
end program explicit_shape_arrays

! Assumed size arrays (with an asterisk) do not have any useful metadata associatd with them in the called subroutine/function.
! Never use them, if possible. Instead, use explicit-shape or assumed-shape arrays instead

program assumed_size_arrays
   implicit none

   integer, parameter :: ARR_SIZE = 5
   integer :: i
   real, dimension(ARR_SIZE) :: a = [1.0, 2.0, 3.0, 4.0, 5.0], b = [(real(i), i=1, ARR_SIZE)]

   call print_array(a, ARR_SIZE)
   call print_array(b, ARR_SIZE)

   call process(a, b, 3)

   call print_array(a, ARR_SIZE)
   call print_array(b, ARR_SIZE)

contains
   subroutine print_array(data, nvals)
      implicit none

      real, dimension(*) :: data
      integer, intent(in) :: nvals
      integer :: j

      write (*, '(*(F8.3))') (data(j), j=1, nvals)
   end subroutine print_array

   subroutine process(data1, data2, nvals)
      implicit none

      real, dimension(*), intent(in) :: data1
      real, dimension(*), intent(out) ::data2
      integer, intent(in) :: nvals
      integer :: j

      do j = 1, nvals
         data2(j) = 3.0*data1(j)
      end do
   end subroutine process
end program assumed_size_arrays

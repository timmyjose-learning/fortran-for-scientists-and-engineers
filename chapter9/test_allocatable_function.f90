module test_module_m
   implicit none
   private

   public :: test_alloc_fun, print_array

contains
   subroutine print_array(arr)
      implicit none

      real, dimension(:), intent(in) :: arr
      integer :: i

      write (*, '(*(F10.2))') (arr(i), i=1, size(arr))
      write (*, *)
   end subroutine print_array

   function test_alloc_fun(n) result(res)
      implicit none

      integer, intent(in) :: n
      real, dimension(:), allocatable :: res
      real, dimension(:), allocatable :: arr
      integer :: stat
      character(len=512) :: errmsg
      integer :: i

      allocate (arr(n), stat=stat, errmsg=errmsg)

      if (stat /= 0) then
         write (*, *) 'Error while allocating array: ', errmsg
         error stop
      end if

      do i = 1, size(arr)
         arr(i) = size(arr) - i + 1
      end do

      res = arr
   end function test_alloc_fun
end module test_module_m

program test_allocatable_function
   use test_module_m, only: test_alloc_fun, print_array
   implicit none

   integer, parameter :: ARR_SIZE = 5
   real, dimension(:), allocatable :: arr

   arr = test_alloc_fun(ARR_SIZE)
   call print_array(arr)

   if (allocated(arr)) then
      deallocate (arr)
   end if
end program test_allocatable_function

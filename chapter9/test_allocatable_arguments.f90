module test_module_m
   implicit none
   private

   public :: test_alloc, print_array
contains

   subroutine print_array(arr)
      implicit none

      real, dimension(:), intent(in) :: arr
      integer :: i

      write (*, '(*(F10.2))') (arr(i), i=1, size(arr))
      write (*, *)
   end subroutine print_array

   subroutine test_alloc(arr)
      implicit none

      real, dimension(:), allocatable, intent(inout) :: arr
      integer :: i
      integer :: stat
      character(len=512) :: errmsg

      if (allocated(arr)) then
         write (*, *) 'Upon entry, the array is already allocated'
         call print_array(arr)
         deallocate (arr)
      else
         write (*, *) 'Upon entry, the array is not allocated'
      end if

      allocate (arr(5), stat=stat, errmsg=errmsg)

      if (stat /= 0) then
         write (*, *) 'Failed to allocate memory: ', errmsg
         error stop
      end if

      do i = 1, size(arr)
         arr(i) = size(arr) - i + 1
      end do
   end subroutine test_alloc
end module test_module_m

program test_allocatable_arguments
   use test_module_m, only: test_alloc, print_array
   implicit none

   real, dimension(:), allocatable :: arr

   allocate (arr(10))
   arr = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
   call test_alloc(arr)

   if (allocated(arr)) then
      call print_array(arr)
      deallocate (arr)
   end if
end program test_allocatable_arguments

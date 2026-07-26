program test_allocatable_arrays
   implicit none

   real, dimension(:), allocatable :: arr1
   real, dimension(8) :: arr2 = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]
   real, dimension(3) :: arr3 = [1.0, -2.0, 3.0]
   integer :: stat

   ! automatically allocates and arr1 is now a 3 element array
   arr1 = 2.0*arr3
   call print_array(arr1)

   ! automatically (re)allocate arr1 as a 4 element array
   arr1 = arr2(1:8:2)
   call print_array(arr1)

   ! re-use arr1 as a 4 element array wihout re-allocating since the size is the same
   arr1 = 2.0*arr2(1:4)
   call print_array(arr1)

   ! deallocate the array
   if (allocated(arr1)) then
      deallocate (arr1, stat=stat)

      if (stat /= 0) then
         error stop 'Could not deallocate memory'
      end if
   end if

contains
   subroutine print_array(a)
      implicit none

      real, dimension(:), intent(in) :: a
      integer :: i

      write (*, '(*(F10.3))') (a(i), i=1, size(a))
      write (*, *)
   end subroutine print_array
end program test_allocatable_arrays

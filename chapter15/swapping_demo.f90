program swapping_demo
   use, intrinsic :: iso_fortran_env, only: output_unit
   implicit none

   real, dimension(2, 3), target :: rarr1, rarr2
   real, dimension(:, :), pointer :: p1 => null(), p2 => null()

   rarr1 = reshape([1.0, 4.0, 2.0, 5.0, 3.0, 6.0], [2, 3])
   rarr2 = reshape([11.0, 44.0, 22.0, 55.0, 33.0, 66.0], [2, 3])

   p1 => rarr1
   p2 => rarr2

   write (output_unit, *) 'Before swapping...'
   call print_array(p1)
   call print_array(p2)

   write (output_unit, *) 'After swapping...'
   call swap_arrays_by_value(rarr1, rarr2)
   call print_array(p1)
   call print_array(p2)

   write (output_unit, *) 'After swapping again...'
   call swap_arrays_using_pointers(p1, p2)
   call print_array(p1)
   call print_array(p2)

contains
   subroutine print_array(arr)
      implicit none

      real, dimension(:,:), intent(in) :: arr
      integer :: i, j

      do i = 1, size(arr, 1)
         write (output_unit, '(*(F8.3,:,". "))') (arr(i,j), j = 1, size(arr, 2))
      end do
      write (output_unit, '(/)')
   end subroutine print_array

   subroutine swap_arrays_by_value(arr1, arr2)
      implicit none

      real, dimension(:,:), intent(inout) :: arr1, arr2
      real, dimension(:,:), allocatable :: temp

      temp = arr1
      arr1 = arr2
      arr2 = temp
   end subroutine swap_arrays_by_value

   subroutine swap_arrays_using_pointers(parr1, parr2)
      implicit none

      ! Important: pointers to arrays must have a deferred shape
      real, dimension(:,:), pointer, intent(inout) :: parr1, parr2
      real, dimension(:,:), pointer :: temp

      temp => parr1
      parr1 => parr2
      parr2 => temp
   end subroutine swap_arrays_using_pointers
end program swapping_demo
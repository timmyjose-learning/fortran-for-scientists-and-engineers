module array_props_m
   implicit none
   private

   public:: print_array_props

contains
   subroutine print_array_props(arr, n, m)
      implicit none

      integer, intent(in) :: n, m
      real, dimension(n, m), intent(in) :: arr

      write (*, *) 'Size = ', size(arr)
      write (*, *) 'Shape = ', shape(arr)
      write (*, *) 'Bounds = ', lbound(arr, 1), ':', ubound(arr, 1), ', and ', lbound(arr, 2), ':', ubound(arr, 2)
      write (*, *)
   end subroutine print_array_props
end module array_props_m

program properties_of_explicit_shape_arrays
   use array_props_m, only: print_array_props
   implicit none

   real, dimension(-5:5, -2:7) :: arr1 = 0.0
   real, dimension(10, 2) :: arr2 = 0.0

   write (*, *) 'Properties of arr1...'
   call print_array_props(arr1, 11, 10)

   write (*, *) 'Properties of arr2...'
   call print_array_props(arr2, 10, 2)
end program properties_of_explicit_shape_arrays


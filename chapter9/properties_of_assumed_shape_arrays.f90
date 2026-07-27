module array_props_m
   implicit none
   private

   public :: print_array_properties

contains
   subroutine print_array_properties(arr)
      implicit none

      real, dimension(:, :), intent(in):: arr

      write (*, *) 'Size = ', size(arr)
      write (*, *) 'Bounds: ', lbound(arr, 1), ':', ubound(arr, 1), ', and ', &
         lbound(arr, 2), ':', ubound(arr, 2)
      write (*, *) 'Shape = ', shape(arr)
      write (*, *)
   end subroutine print_array_properties
end module array_props_m

program properties_of_assumed_shape_arrays
   use array_props_m, only: print_array_properties
   implicit none

   real, dimension(-5:5, -2:7) :: arr1 = 0.0
   real, dimension(10, 2) :: arr2 = 0.0

   write (*, *) 'Properties for array arr1:'
   call print_array_properties(arr1)

   write (*, *) 'Properties for array arr2:'
   call print_array_properties((arr2))
end program properties_of_assumed_shape_arrays


! Fortran cannot natively express the concept of an array of pointers.
! However, we can simulate it using derived types.

program array_of_pointers
   use, intrinsic :: iso_fortran_env, only: output_unit
   implicit none

   type :: ptr_t
      real, dimension(:), pointer :: p => null()
   end type ptr_t

   real, dimension(1), target :: arr1 = [1.0]
   real, dimension(2), target :: arr2 = [1.0, 2.0]
   real, dimension(3), target :: arr3 = [1.0, 2.0, 3.0]
   real, dimension(4), target :: arr4 = [1.0, 2.0, 3.0, 4.0]
   real, dimension(5), target :: arr5 = [1.0, 2.0, 3.0, 4.0, 5.0]

   type(ptr_t), dimension(5) :: arr_ptr

   arr_ptr(1)%p => arr1
   arr_ptr(2)%p => arr2
   arr_ptr(3)%p => arr3
   arr_ptr(4)%p => arr4
   arr_ptr(5)%p => arr5

   arr_ptr(3)%p(1) = 100.0

   write (output_unit, '(*(F8.3,:", "))') arr3
end program array_of_pointers
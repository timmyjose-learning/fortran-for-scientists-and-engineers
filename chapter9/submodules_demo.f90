module array_utils_m
   implicit none
   private

   type :: alloc_fun_ret_t
      real, dimension(:), allocatable :: arr
   end type alloc_fun_ret_t

   interface
      module type(alloc_fun_ret_t) pure function test_alloc_fun(n) result(res)
         implicit none
         integer, intent(in):: n
      end function test_alloc_fun

      module subroutine print_array(arr)
         implicit none
         real, dimension(:), intent(in) :: arr
      end subroutine print_array
   end interface

   public :: test_alloc_fun, print_array, alloc_fun_ret_t
end module array_utils_m

submodule(array_utils_m) array_utils_impl_m
   implicit none

contains
   module procedure test_alloc_fun
   implicit none

   !real, dimension(:), allocatable :: res
   integer :: i

   allocate (res%arr(n))

   do i = 1, size(res%arr)
      res%arr(i) = real(size(res%arr) - i + 1)
   end do
   end procedure test_alloc_fun

   module procedure print_array
   implicit none

   integer :: i

   write (*, '(*(F10.2))') (arr(i), i=1, size(arr))
   write (*, *)
   end procedure print_array
end submodule array_utils_impl_m

program submodules_demo
   use array_utils_m, only: alloc_fun_ret_t, test_alloc_fun, print_array
   implicit none

   type(alloc_fun_ret_t) :: arr_wrapper

   arr_wrapper = test_alloc_fun(10)
   call print_array(arr_wrapper%arr)

   if (allocated(arr_wrapper%arr)) then
      deallocate (arr_wrapper%arr)
   end if
end program submodules_demo

module vector_m
   use, intrinsic :: iso_fortran_env, only: real64
   implicit none
   private

   integer, parameter :: REAL_K = real64

   type :: vector_t
      real(kind=REAL_K) :: x
      real(kind=REAL_K) :: y

   contains
      procedure, pass :: add, sub
   end type vector_t

   public :: vector_t

contains
   function add(this, other) result(sum)
      implicit none

      class(vector_t) :: this, other
      type(vector_t) :: sum

      sum%x = this%x + other%x
      sum%y = this%y + other%y
   end function add

   function sub(this, other) result(diff)
      implicit none

      class(vector_T) :: this, other
      type(vector_t) :: diff

      diff%x = this%x - other%x
      diff%y = this%y - other%y
   end function sub
end module vector_m

program vector_type_bound_demo
   use, intrinsic :: iso_fortran_env, only: input_unit, output_unit
   use vector_m, only: vector_t
   implicit none

   type(vector_t) :: v1, v2, v3

   write (output_unit, *) 'Enter the first vector'
   read (input_unit, *) v1%x, v1%y

   write (output_unit, *) 'Enter the second vector'
   read (input_unit, *) v2%x, v2%y

   v3 = v1%add(v2)
   write (output_unit, *) 'Sum = ', v3

   v3 = v1%sub(v2)
   write (output_unit, *) 'Difference = ', v3
end program vector_type_bound_demo
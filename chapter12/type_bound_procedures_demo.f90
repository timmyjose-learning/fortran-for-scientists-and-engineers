module point_m
   use, intrinsic :: iso_fortran_env, only: real64
   implicit none
   private

   integer, parameter :: REAL_K = real64

   type :: point_t
      real(kind=REAL_K) :: x
      real(kind=REAL_K) :: y

   contains
      procedure, pass :: add, sub
   end type point_t

   public :: REAL_K, point_t

contains
   function add(this, other) result(point)
      implicit none

      class(point_t) :: this, other
      type(point_t) :: point

      point%x = this%x + other%x
      point%y = this%y + other%y
   end function add

   function sub(this, other) result(point)
      implicit none

      class(point_t) :: this, other
      type(point_t) :: point

      point%x = this%x - other%x
      point%y = this%y - other%y
   end function sub
end module point_m

program type_bound_procedures_demo
   use, intrinsic :: iso_fortran_env, only: input_unit, output_unit
   use point_m, only: point_t
   implicit none

   type(point_t) :: p1, p2
   type(point_t) :: p3

   write (output_unit, *) 'Enter the first point'
   read (input_unit, *) p1%x, p1%y

   write (output_unit, *) 'Enter the second point'
   read (input_unit, *) p2%x, p2%y

   p3 = p1%add(p2)
   write (output_unit, *) 'Sum = ', p3

   p3 = p1%sub(p2)
   write (output_unit, *) 'Difference = ', p3
end program type_bound_procedures_demo
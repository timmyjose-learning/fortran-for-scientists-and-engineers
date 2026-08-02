module vector_m
   use, intrinsic :: iso_fortran_env, only: real64
   implicit none
   private

   type :: vector_t
      real(kind=real64) :: x
      real(kind=real64) :: y

   contains
      generic :: add => add_vector_vector, add_vector_scalar
      procedure, pass :: add_vector_vector
      procedure, pass :: add_vector_scalar
   end type vector_t

   public :: vector_t

contains
   function add_vector_vector(this, other) result(sum)
      implicit none

      class(vector_t), intent(in) :: this,other
      type(vector_t) :: sum

      sum%x = this%x + other%x
      sum%y = this%y + other%y
   end function add_vector_vector

   function add_vector_scalar(this, other) result(sum)
      implicit none

      class(vector_t), intent(in) :: this
      real(kind=real64),intent(in) :: other
      type(vector_t) :: sum

      sum%x = this%x + other
      sum%y = this%y + other
   end function add_vector_scalar
end module vector_m

program generic_bound_procedures_demo
   use, intrinsic :: iso_fortran_env, only: input_unit, output_unit, real64
   use vector_m, only: vector_t
   implicit none

   call run_app()

contains
   subroutine run_app()
      implicit none

      type(vector_t) :: v1, v2

      write (output_unit, *) 'Enter the first vector'
      read (input_unit, *) v1%x, v1%y

      write (output_unit, *) 'Enter the secon vector'
      read (input_unit, *) v2%x, v2%y

      write (output_unit, *) 'Sum of the two vectors = ', v1%add(v2)
      write (output_unit, *) 'v1 + 10 = ', v1%add(10.0_real64)
      write (output_unit, *) 'v2 + 12 = ', v2%add(12.0_real64)
   end subroutine run_app
end program generic_bound_procedures_demo
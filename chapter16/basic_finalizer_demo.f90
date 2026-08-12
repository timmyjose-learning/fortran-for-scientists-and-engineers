module person_m
   implicit none

   type :: person_t
      private
      character(len=:), allocatable :: name
      integer :: age
   contains
      ! finalizers are not type-bound procedues, and so cannot use aliasing like so:
      ! final :: clean => clean_person
      final :: clean_person
   end type person_t

   public :: person_t, make_person

contains
   function make_person(name, age) result(person)
      implicit none

      character(len=*), intent(in) :: name
      integer, intent(in) :: age
      type(person_t) :: person

      person%name = name
      person%age = age
   end function make_person

   subroutine clean_person(this)
      implicit none

      ! finalizers are not polymorphic, and must have the exact type - hence the `type` instead of `class`
      type(person_t), intent(inout) :: this

      write (*, *) 'Cleaning up ', this%name
   end subroutine clean_person
end module person_m


program basic_finalizer_demo
   use, intrinsic :: iso_fortran_env, only: output_unit
   use person_m, only: person_t, make_person

   call run_app()

contains
   subroutine run_app()
      implicit none

      type(person_t) :: bob

      bob = make_person('Bob', 42)
   end subroutine run_app
end program basic_finalizer_demo
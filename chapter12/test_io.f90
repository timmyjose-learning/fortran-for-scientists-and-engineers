module person_m
   use, intrinsic :: iso_fortran_env, only: output_unit
   implicit none
   private

   type :: person_t
      character(len=:), allocatable :: first_name
      character :: middle_initial
      character(len=:), allocatable :: last_name
      character(len=:), allocatable :: phone
      integer :: age
      character :: sex
      character(len=:), allocatable :: ssn

   contains
      procedure write_person_t
         generic :: write(formatted) => write_person_t
      end type person_t

      public :: person_t

   contains
      subroutine write_person_t(p, unit, iotype, v_list, iostat, iomsg)
         implicit none

         class(person_t), intent(in) :: p
         integer, intent(in) :: unit
         character(len=*), intent(in) :: iotype
         integer, intent(in) :: v_list(:)
         integer, intent(out) :: iostat
         character(len=*), intent(inout) :: iomsg

         ! Silence compiler unused warnings
         if (.false.) then
            if (len(iotype) == 0) continue
            if (size(v_list) == 0) continue
         end if

         write(unit, '("{",/,T5,3(A,/, T5),T5,A,/,T5,I0,/,T5,A,/,T5,A,/,"}")', iostat=iostat, iomsg=iomsg) &
            p%first_name, p%middle_initial, p%last_name, p%phone, p%age, p%sex, p%ssn
      end subroutine write_person_t
   end module person_m

   program test_io
      use, intrinsic :: iso_fortran_env, only: output_unit
      use person_m, only: person_t
      implicit none

      call run_app()

   contains
      subroutine run_app()
         implicit none
         type(person_t) :: john

         john = person_t('John', 'R', 'Jones', '323-6439', 21, 'M', '123-45-6789')

         write (output_unit, *) john

      end subroutine run_app
   end program test_io
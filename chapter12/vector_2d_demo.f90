module vector_m
   use, intrinsic :: iso_fortran_env, only: real64
   implicit none
   private

   integer, parameter :: REAL_K = real64

   type :: vector_t
      real(kind=REAL_K) :: x
      real(kind=REAL_K) :: y

   contains
      procedure write_vector_t
         generic :: write(formatted) => write_vector_t
      end type vector_t


      public :: REAL_K, vector_t, add_vectors, sub_vectors

   contains
      function add_vectors(v1, v2) result(sum)
         implicit none

         type(vector_t), intent(in) :: v1, v2
         type(vector_t) :: sum

         sum%x = v1%x + v2%x
         sum%y = v1%y + v2%y
      end function add_vectors

      function sub_vectors(v1, v2) result(diff)
         implicit none

         type(vector_t), intent(in) :: v1, v2
         type(vector_t) :: diff

         diff%x = v1%x - v2%x
         diff%y = v1%y - v2%y
      end function sub_vectors

      subroutine write_vector_t(vec, unit, iotype, vlist, iostat, iomsg)
         implicit none

         class(vector_t), intent(in) :: vec
         integer, intent(in) :: unit
         character(len=*), intent(in) :: iotype
         integer, dimension(:), intent(in) :: vlist
         integer, intent(out) :: iostat
         character(len=*), intent(inout) :: iomsg
         character(len=24) :: xstr, ystr

         if (.false.) then
            if (len(iotype) == 0) return
            if (size(vlist) == 0) return
         end if

         write (xstr, '(F8.3)') vec%x
         write (ystr, '(F8.3)') vec%y

         write (unit, '("(",A,",",A,")",/)', iostat=iostat, iomsg=iomsg) trim(adjustl(xstr)), trim(adjustl(ystr))
      end subroutine write_vector_t
   end module vector_m

   program vector_2d_demo
      use, intrinsic :: iso_fortran_env, only: input_unit, output_unit
      use vector_m, only: vector_t, add_vectors, sub_vectors
      implicit none

      call run_app()

   contains
      subroutine run_app()
         implicit none

         type(vector_t) :: vec1, vec2

         write (output_unit, *) 'Enter the first vector''s x and y fields'
         read (input_unit, *) vec1%x, vec1%y

         write (output_unit, *) 'Enter the second vector''s x and y fields'
         read (input_unit, *) vec2%x, vec2%y

         write (output_unit, *) 'Sum = ', add_vectors(vec1, vec2)
         write (output_unit, *) 'Difference = ', sub_vectors(vec1, vec2)
      end subroutine run_app
   end program vector_2d_demo
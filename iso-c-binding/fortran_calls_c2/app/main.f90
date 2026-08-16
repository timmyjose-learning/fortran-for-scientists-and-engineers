program main
   use, intrinsic :: iso_fortran_env, only: output_unit
   use, intrinsic :: iso_c_binding, only: c_int, c_float, c_char, c_null_char
   use sub_m, only: my_type_t, c_sub
   implicit none

   call run_app()

contains
   subroutine run_app()
      implicit none

      type(my_type_t) :: obj
      character(kind=c_char), dimension(20) :: msg

      obj%n = 3_c_int
      obj%data1 = 6.0_c_float
      obj%data2 = 0.0_c_float

      msg(1) = 'H'
      msg(2) = 'e'
      msg(3) = 'l'
      msg(4) = 'l'
      msg(5) = 'o'
      msg(6) = c_null_char

      write (output_unit, *) 'Before the call...'
      write (output_unit, *) 'n = ', obj%n
      write (output_unit, *) 'data1 = ', obj%data1
      write (output_unit, *) 'data2 = ', obj%data2

      call c_sub(obj, msg)

      write (output_unit, *) 'After the call...'
      write (output_unit, *) 'n = ', obj%n
      write (output_unit, *) 'data1 = ', obj%data1
      write (output_unit, *) 'data2 = ', obj%data2
   end subroutine run_app
end program main

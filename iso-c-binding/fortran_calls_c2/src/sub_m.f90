module sub_m
   use, intrinsic :: iso_c_binding, only: c_int, c_float, c_char, c_null_char
   implicit none
   private

   type, bind(C) :: my_type_t
      integer(kind=c_int) :: n
      real(kind=c_float) :: data1
      real(kind=c_float) :: data2
   end type my_type_t

   interface
      subroutine c_sub(mytype, msg) bind(C)
         import :: my_type_t, c_char
         implicit none

         type(my_type_t) :: mytype
         character(kind=c_char), dimension(20) :: msg
      end subroutine c_sub
   end interface

   public :: my_type_t, c_sub
end module sub_m

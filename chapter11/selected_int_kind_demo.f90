program selected_int_kind_demo
   implicit none

   integer, parameter :: SHORT = selected_int_kind(3) ! range up to 10^3, so 16 bits or 2 bytes will suffice
   integer, parameter :: INTEGER = selected_int_kind(9) ! should be 32 bits or 4 bytes
   integer, parameter :: LONG = selected_int_kind(14) ! should be 64 bits or 8 bytes

   write (*, *) 'Short = ', SHORT, ', INTEGER = ', INTEGER, ', and LONG = ', LONG
end program selected_int_kind_demo
program character_kind_demo
   implicit none

   integer, parameter :: DEFAULt_K = selected_char_kind('DEFAULT')
   integer, parameter :: ASCII_K = selected_char_kind('ASCII')
   integer, parameter :: UNICODE_K = selected_char_kind('ISO_10646')

   write (*, *) DEFAULt_K, ASCII_K, UNICODE_K
end program character_kind_demo

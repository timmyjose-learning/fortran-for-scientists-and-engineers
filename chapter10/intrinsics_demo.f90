program intrinsics_demo
   implicit none

   character :: c
   integer :: i
   character(len=512), parameter :: str = 'Hello, world!'
   character(len=:), allocatable :: trimmed_str

   c = 'A'
   write (*, *) 'The character ', c, ' has ASCII code ', iachar(c)

   i= 121
   write (*, *) 'ASCII code ', i, ' is the character ', achar(i)

   write (*, *) 'Length = ', len(str)
   write (*, *) 'Trimmed length  ', len_trim(str)

   write (*, *) 'Index of e = ', index(str, 'e')
   write (*, *) 'Index of x = ', index(str, 'x')
   write (*, *) 'Reverse index of l = ', index(str, 'l', .true.) ! the last arg is `back`
   write (*, *) 'Reverse index of x = ', index(str, 'x', .true.)

   trimmed_str = trim(str)

   write (*, *) 'Length = ', len(trimmed_str)
   write (*, *) 'Trimmed length = ', len_trim(trimmed_str)

   if (allocated(trimmed_str)) then
      deallocate(trimmed_str)
   end if
end program intrinsics_demo
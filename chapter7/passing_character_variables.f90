program passing_character_variables
   implicit none

   character(len=1) :: a = 'x'
   character(len=:), allocatable :: b
   character(len=512) :: c = 'Hello, world!'

   allocate (character(len=512) :: b)
   read (*, *) b

   call print_string(a)
   call print_string(b)
   call print_string(c)

   if (allocated(b)) then
      deallocate (b)
   end if

contains
   subroutine print_string(string)
      implicit none

      character(len=*) :: string

      write (*, '(A,I5)') 'The len of the string is ', len(string)
   end subroutine print_string
end program passing_character_variables

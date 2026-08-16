module hello_m
   implicit none

   interface
     subroutine say_hello() bind(c)
       end subroutine
   end interface
end module hello_m

program main
   use hello_m, only: say_hello
   implicit none

   call say_hello()
end program main

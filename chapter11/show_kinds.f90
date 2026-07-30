program show_kinds
   implicit none

   call main()

contains
   subroutine main()
      implicit none

      real(kind=4) :: a
      real(kind=8) :: b
      real(kind=16) :: c

      write (*, *) 'Kind = ', kind(a)
      write (*, *) 'Kind = ', kind(b)
      write (*, *) 'Kind = ', kind(c)
   end subroutine main
end program show_kinds
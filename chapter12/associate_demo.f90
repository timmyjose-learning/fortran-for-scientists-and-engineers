program associate_demo
   implicit none

   type :: point_t
      real :: x
      real :: y
   end type point_t

   call run_app()

contains
   subroutine run_app()
      implicit none

      type(point_t) :: p1, p2
      type(point_t) :: p3

      write (*, *) 'Enter the first point'
      read (*, *) p1%x, p1%y

      write (*, *) 'Enter the second point'
      read (*, *) p2%x, p2%y

      associate (x1 => p1%x,  y1 => p1%y, x2 => p2%x, y2 => p2%y)
         p3%x = x1 + x2
         p3%y = y1 + y2
      end associate

      write (*,*) 'Sum = ', p3
   end subroutine run_app
end program associate_demo

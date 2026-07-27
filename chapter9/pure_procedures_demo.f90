program pure_procedures_demo
   implicit none

   type :: point_t
      real :: x
      real :: y
   end type point_t

   type(point_t) :: p1, p2

   p1%x = 0.0
   p1%y = 0.0

   p2%x = 3.0
   p2%y = 4.0

   write (*, *) 'Dist = ', length(p1, p2)

contains
   real pure function length(point1, point2)
      implicit none

      type(point_t), intent(in) :: point1, point2
      real :: deltax, deltay
      real :: dist

      deltax = point2%x - point1%x
      deltay = point2%y - point1%y
      dist = sqrt(deltax**2 + deltay**2)

      length = dist
   end function length
end program pure_procedures_demo

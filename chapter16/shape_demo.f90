module constants_m
   implicit none
   private

   real, parameter :: PI = 3.141593

   public :: PI
end module constants_m

module shape_m
   implicit none
   private

   type, abstract :: shape_t
      private
      integer :: id
   contains
      procedure, pass(this) :: get_id
      procedure(unary_real_fn), pass(this), deferred :: area
      procedure(unary_real_fn), pass(this), deferred :: perimeter
   end type shape_t

   abstract interface
      function unary_real_fn(this) result(res)
         import :: shape_t
         implicit none

         class(shape_t), intent(in) :: this
         real :: res
      end function unary_real_fn
   end interface

   public :: shape_t

contains
   function get_id(this) result(id)
      implicit none

      class(shape_t), intent(in) :: this
      integer :: id

      id = this%id
   end function get_id
end module shape_m

module circle_m
   use shape_m, only: shape_t
   use constants_m, only: PI
   implicit none
   private

   type, extends(shape_t) :: circle_t
      private
      real :: radius = 0.0

   contains
      procedure, pass(this) :: get_radius
      procedure, pass(this) :: area => get_circle_area
      procedure, pass(this) :: perimeter => get_circle_perimeter
   end type circle_t

   public :: circle_t, make_circle

contains
   function make_circle(radius) result(circle)
      implicit none

      real, intent(in) :: radius
      type(circle_t) :: circle

      circle%radius = radius
   end function make_circle

   function get_radius(this) result(radius)
      implicit none

      class(circle_t), intent(in) :: this
      real :: radius

      radius = this%radius
   end function get_radius

   function get_circle_area(this) result(area)
      implicit none

      class(circle_t), intent(in) :: this
      real :: area

      area = PI * this%radius * this%radius
   end function get_circle_area

   function get_circle_perimeter(this) result(perimeter)
      implicit none

      class(circle_t), intent(in) :: this
      real :: perimeter

      perimeter = 2.0 * PI * this%radius
   end function get_circle_perimeter
end module circle_m

module rect_m
   use shape_m, only: shape_t
   implicit none
   private

   type, extends(shape_t) :: rect_t
      private
      real :: length
      real :: breadth

   contains
      procedure, pass(this) :: get_length
      procedure, pass(this) :: get_breadth
      procedure, pass(this) :: area => get_rect_area
      procedure, pass(this) :: perimeter => get_rect_perimeter
   end type rect_t

   public :: rect_t, make_rect

contains
   function make_rect(length, breadth) result(rect)
      implicit none

      real, intent(in) :: length
      real, intent(in) :: breadth
      type(rect_t) :: rect

      rect%length = length
      rect%breadth = breadth
   end function make_rect

   function get_length(this) result(length)
      implicit none

      class(rect_t), intent(in) :: this
      real :: length

      length = this%length
   end function get_length

   function get_breadth(this) result(breadth)
      implicit none

      class(rect_t), intent(in) :: this
      real :: breadth

      breadth = this%breadth
   end function get_breadth

   function get_rect_area(this) result(area)
      implicit none

      class(rect_t), intent(in) :: this
      real :: area

      area = this%length * this%breadth
   end function get_rect_area

   function get_rect_perimeter(this) result(perimeter)
      implicit none

      class(rect_t), intent(in) :: this
      real :: perimeter

      perimeter = 2.0 * (this%length + this%breadth)
   end function get_rect_perimeter
end module rect_m

program shape_demo
   use, intrinsic :: iso_fortran_env, only: input_unit, output_unit, error_unit
   use shape_m, only: shape_t
   use circle_m, only: circle_t, make_circle
   use rect_m, only: rect_t, make_rect
   implicit none

   call run_app()

contains
   subroutine run_app()
      implicit none

      class(shape_t), pointer :: p
      type(circle_t), target :: circle
      type(rect_t), target :: rect

      circle = make_circle(10.0)
      p => circle
      call dispatch(p)

      rect = make_rect(10.0, 20.0)
      p => rect
      call dispatch(p)
   end subroutine run_app

   subroutine dispatch(shape)
      implicit none

      class(shape_t), pointer, intent(in) :: shape
      select type (shape)
       type is (circle_t)
         write (output_unit, '("Area of circle with radius ",F8.3," is ",F8.3,", and perimeter is ",F8.3)') shape%get_radius(), &
            shape%area(), shape%perimeter()
       type is (rect_t)
         write (output_unit, '("Area of rectangle with length ",F8.3, " and breadth",F8.3, " is ",F8.3,", and perimeter is ",F8.3)') &
            shape%get_length(), shape%get_breadth(), shape%area(), shape%perimeter()
       class default
         write (error_unit, *) 'Unknown type'
      end select
   end subroutine dispatch
end program shape_demo
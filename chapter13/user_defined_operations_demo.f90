module vector_m
   implicit none
   private

   type :: vector_t
      real :: x
      real :: y
      real :: z
   end type vector_t

   interface assignment (=)
      module procedure array_to_vector
      module procedure vector_to_array
   end interface

   interface operator (+)
      module procedure vector_add
   end interface

   interface operator (-)
      module procedure vector_subtract
   end interface

   interface operator (*)
      module procedure vector_times_real
      module procedure vector_times_int
      module procedure real_times_vector
      module procedure int_times_vector
      module procedure cross_product
   end interface

   interface operator (/)
      module procedure vector_div_real
      module procedure vector_div_int
   end interface

   interface operator (.DOT.)
      module procedure vec_dot_product
   end interface

   public :: vector_t, assignment(=), operator(+), operator(-), operator(*), operator(/), operator(.DOT.)

contains
   subroutine array_to_vector(vec_result, array)
      implicit none

      type(vector_t), intent(out) :: vec_result
      real, dimension(3), intent(in) :: array

      if (size(array) < 3) then
         error stop 'array is too small'
      end if

      vec_result%x = array(1)
      vec_result%y = array(2)
      vec_result%z = array(3)
   end subroutine array_to_vector

   subroutine vector_to_array(array_result, vec)
      implicit none

      real, dimension(3), intent(out) :: array_result
      type(vector_t), intent(in) :: vec

      if (size(array_result) /= 3) then
         error stop 'array must be at least size 3'
      end if

      array_result(1) = vec%x
      array_result(2) = vec%y
      array_result(3) = vec%z
   end subroutine vector_to_array

   function vector_add(v1, v2) result(sum)
      implicit none
      type(vector_t), intent(in) :: v1, v2
      type(vector_t) :: sum

      sum%x = v1%x + v2%x
      sum%y = v1%y + v2%y
      sum%z = v1%z + v2%z
   end function vector_add

   function vector_subtract(v1, v2) result(diff)
      implicit none
      type(vector_t), intent(in) :: v1, v2
      type(vector_t) :: diff

      diff%x = v1%x - v2%x
      diff%y = v1%y - v2%y
      diff%z = v1%z - v2%z
   end function vector_subtract

   function vector_times_real(v, r) result(prod)
      implicit none

      type(vector_t), intent(in) :: v
      real, intent(in) :: r
      type(vector_t) :: prod

      prod%x = v%x * r
      prod%y = v%y * r
      prod%z = v%z * r
   end function vector_times_real

   function vector_times_int(v, d) result(prod)
      implicit none

      type(vector_t), intent(in) :: v
      integer, intent(in) :: d
      type(vector_t) :: prod

      prod%x = v%x * d
      prod%y = v%y * d
      prod%z = v%z * d
   end function vector_times_int

   function real_times_vector(r, v) result(prod)
      implicit none

      real, intent(in) :: r
      type(vector_t), intent(in) :: v
      type(vector_t) :: prod

      prod%x = r * v%x
      prod%y = r * v%y
      prod%z = r * v%z
   end function real_times_vector

   function int_times_vector(d, v) result(prod)
      implicit none

      integer, intent(in) :: d
      type(vector_t), intent(in) :: v
      type(vector_t) :: prod

      prod%x = d * v%x
      prod%y = d * v%y
      prod%z = d * v%z
   end function int_times_vector

   function cross_product(v1, v2) result(prod)
      implicit none

      type(vector_t), intent(in) :: v1, v2
      type(vector_t) :: prod

      prod%x = v1%y * v2%y + v1%z * v2%z
      prod%y = v1%x * v2%x + v1%z * v2%z
      prod%z = v1%x * v2%x + v1%y * v2%y
   end function cross_product

   function vector_div_real(v, r) result(quot)
      implicit none

      type(vector_t), intent(in) :: v
      real, intent(in) :: r
      type(vector_t) :: quot

      quot%x = v%x / r
      quot%y = v%y / r
      quot%z = v%z / r
   end function vector_div_real

   function vector_div_int(v, d) result(quot)
      implicit none

      type(vector_t), intent(in) :: v
      integer, intent(in) :: d
      type(vector_t) :: quot

      quot%x = v%x / d
      quot%y = v%y / d
      quot%z = v%z / d
   end function vector_div_int

   function vec_dot_product(v1, v2) result(prod)
      implicit none

      type(vector_t), intent(in) :: v1, v2
      real :: prod

      prod = v1%x * v2%x + v1%y * v2%y + v1%z * v2%z
   end function vec_dot_product
end module vector_m

program user_defined_operations_demo
   use vector_m, only: vector_t, assignment(=), operator(+), operator(-), operator(*), operator(/), operator(.DOT.)
   implicit none

   type (vector_t) :: v1, v2
   real, dimension(3) :: array_out

   v1 = (/ 1.0, 2.0, 3.0 /)
   array_out = v1
   v2 = v1 * 10.0

   write (*, *) 'array_out ', array_out
   write (*, *) 'v1 = ', v1
   write (*, *) 'v2 = ', v2

   write (*, *) 'v1 + v2 = ', v1 + v2
   write (*, *) 'v1 - v2 = ', v1 - v2
   write (*, *) 'v1 * v2 = ', v1 * v2
   write (*, *) 'v1 / 10 = ', v1 / 10
   write (*, *) 'v1 / 5.2 = ', v1 / 5.2
   write (*, *) 'v1 .DOT. v2 = ', v1 .DOT. v2
end program user_defined_operations_demo
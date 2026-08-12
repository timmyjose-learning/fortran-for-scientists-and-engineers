module timer_m
   use, intrinsic :: iso_c_binding, only: c_int64_t, c_ptr, c_null_ptr
   implicit none
   private

   interface
      function get_c_epoch(timer) result(time) bind(c, name='time')
         import :: c_int64_t, c_ptr

         type(c_ptr), value :: timer
         integer(kind=c_int64_t) :: time
      end function get_c_epoch
   end interface

   type :: timer_t
      private
      integer(c_int64_t) :: start_time = 0_c_int64_t

   contains
      procedure, pass(this) :: start => start_timer
      procedure, pass(this) :: elapsed => elapsed_time
   end type timer_t

   ! good practice
   private :: start_timer, elapsed_time
   public :: timer_t

contains
   subroutine start_timer(this)
      implicit none

      class(timer_t), intent(inout) :: this

      this%start_time = get_c_epoch(c_null_ptr)
   end subroutine start_timer

   function elapsed_time(this) result(duration)
      implicit none

      class(timer_t), intent(in) :: this
      integer(c_int64_t) :: duration

      duration = get_c_epoch(c_null_ptr) - this%start_time
   end function elapsed_time
end module timer_m

program timer_demo
   use, intrinsic :: iso_fortran_env, only: output_unit
   use timer_m, only: timer_t
   implicit none

   integer, parameter :: MAX_ITER = 100000
   type(timer_t) :: timer
   integer :: i, j, k

   call timer%start()

   do i = 1, MAX_ITER
      do j = 1, MAX_ITER
         k = i + j
      end do
   end do

   write (output_unit, '("Elapsed time = ",I5)') timer%elapsed()
end program timer_demo
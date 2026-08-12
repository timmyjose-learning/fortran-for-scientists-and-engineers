module vector_m
   use, intrinsic :: iso_fortran_env, only: error_unit
   implicit none
   private

   type :: vector_t
      private
      real, dimension(:), pointer :: v => null()
      logical :: allocated = .false.

   contains
      procedure :: set => set_vector_using_array
      procedure :: get => get_vector_as_array
      final :: cleanup_vector
   end type vector_t

   private :: set_vector_using_array, get_vector_as_array
   public :: vector_t

contains
   subroutine set_vector_using_array(this, array)
      implicit none

      class(vector_t), intent(inout) :: this
      real, dimension(:), intent(in) :: array
      integer :: stat
      character(len=512) :: errmsg

      if (this%allocated .eqv. .true.) then
         deallocate(this%v, stat=stat, errmsg=errmsg)
         if (stat /= 0) then
            write (error_unit, *) 'Failed to deallocate existing vector: ' // errmsg
            error stop
         end if
         this%allocated = .false.
      end if

      allocate(this%v(size(array, 1)), stat=stat, errmsg=errmsg)
      if (stat /= 0) then
         write (error_unit, *) 'Failed to allocate vector: ' // errmsg
         error stop
      end if

      this%v = array
      this%allocated = .true.
   end subroutine set_vector_using_array

   subroutine get_vector_as_array(this, array)
      implicit none

      class(vector_t), intent(in) :: this
      real, dimension(:), intent(out) :: array
      integer :: vlen = 0
      integer :: alen = 0

      if (this%allocated) then
         vlen = size(this%v, 1)
         alen = size(array, 1)

         if (vlen <= alen) then
            array(1:vlen) = this%v
            array(vlen + 1) = 0.0
         else
            array(1:alen) = this%v(1:alen)
         end if
      else
         array = 0.0
      end if
   end subroutine get_vector_as_array

   subroutine cleanup_vector(this)
      implicit none

      type(vector_t), intent(inout) :: this
      integer :: stat
      character(len=512) :: errmsg

      if (this%allocated .eqv. .true.) then
         deallocate(this%v, stat=stat, errmsg=errmsg)
         if (stat /= 0) then
            write (error_unit, *) 'Error during deallocation: ' // errmsg
         end if
      end if
   end subroutine cleanup_vector
end module vector_m

program vector_finalizer_demo
   use, intrinsic :: iso_fortran_env, only: output_unit
   use vector_m, only: vector_t
   implicit none

   call run_app()

contains
   subroutine run_app()
      implicit none

      type(vector_t) :: vec
      real, dimension(5) :: small_array
      real, dimension(12) :: large_array

      call vec%set([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0])

      call vec%get(small_array)
      write (output_unit, *) 'small_array = ', small_array

      call vec%get(large_array)
      write (output_unit, *) 'large_array = ', large_array

   end subroutine run_app
end program vector_finalizer_demo
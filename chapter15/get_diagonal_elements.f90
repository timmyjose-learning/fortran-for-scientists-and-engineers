module assert_m
   use, intrinsic :: iso_fortran_env, only: error_unit
   implicit none
   private

   public :: assert_true

contains
   subroutine assert_true (ok, message)
      implicit none

      logical, intent(in) :: ok
      character(len=*), intent(in), optional :: message

      if (.not. ok) then
         if (present(message)) then
            write (error_unit, *) 'Assertion failed: ' // message
         else
            write (error_unit, *) 'Assertion failed'
         end if
      end if
   end subroutine assert_true
end module assert_m

program get_diagonal_elements
   use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
   use assert_m, only: assert_true
   implicit none

   call run_app()

contains
   subroutine run_app()
      implicit none

      integer, dimension(:,:), pointer :: arr
      integer, dimension(:),  pointer :: diag
      integer :: error
      integer :: stat
      character(len=512) :: errmsg

      ! trigger error 1
      call get_diagonal(arr, diag, error)
      call assert_true (error == 1, 'no pointers allocated')

      ! trigger error 3
      allocate(arr(3, 4), stat=stat, errmsg=errmsg)
      call handle_alloc_error(stat, errmsg)

      deallocate(diag, stat=stat, errmsg=errmsg)
      call handle_alloc_error(stat, errmsg)

      call get_diagonal(arr, diag, error)
      call assert_true(error == 3, 'array not square')

      ! trigger error 2
      deallocate(arr, stat=stat, errmsg=errmsg)
      call handle_alloc_error(stat, errmsg)

      allocate(arr(3,3), stat=stat, errmsg=errmsg)
      call handle_alloc_error(stat, errmsg)

      allocate(diag(3), stat=stat, errmsg=errmsg)
      call handle_alloc_error(stat, errmsg)

      arr = reshape([1, 4, 7, 2, 5, 8, 3, 6, 9], [3, 3])
      call get_diagonal(arr, diag, error)
      call assert_true (error == 2, 'both pointers allocated')


      deallocate(diag, stat=stat, errmsg=errmsg)
      call handle_alloc_error(stat, errmsg)

      call get_diagonal(arr, diag, error)
      call print_array(diag)

      if (associated(arr)) then
         deallocate(arr, stat=stat, errmsg=errmsg)
         call handle_alloc_error(stat, errmsg)
      end if

      if (associated(diag)) then
         deallocate(diag, stat=stat, errmsg=errmsg)
         call handle_alloc_error(stat, errmsg)
      end if
   end subroutine run_app

   subroutine handle_alloc_error(stat, msg)
      implicit none

      integer, intent(in) :: stat
      character(len=*), intent(in), optional :: msg

      if (stat /= 0) then
         if (present(msg)) then
            write (error_unit, *) 'Allocation/deallocate error: ' // msg
         else
            write (error_unit, *) 'Allocation/deallocate error'
         end if
      end if
   end subroutine handle_alloc_error


   ! error:
   ! 0 -> no error
   ! 1 -> ptr_a not associated
   ! 2 -> ptr_b already associated
   ! 3 -> ptr_a array not square
   ! 4 -> unable to allocate memory for ptr_b
   subroutine get_diagonal(ptr_a, ptr_b, error)
      implicit none

      integer, dimension(:,:), pointer, intent(in) :: ptr_a
      integer, dimension(:), pointer, intent(inout) :: ptr_b
      integer, intent(out) :: error
      integer, dimension(2) :: lb, ub, extent
      integer :: stat

      if (.not. associated (ptr_a)) then
         error = 1
         return
      end if

      if (associated(ptr_b)) then
         error = 2
         return
      end if

      lb = lbound(ptr_a)
      ub = ubound(ptr_a)
      extent = ub - lb + 1

      ! not a square array
      if (extent(1) /= extent(2)) then
         error = 3
         return
      end if

      ! allocate memory for the diagonal array
      ! and fill it in
      allocate(ptr_b(extent(1)), stat=stat)
      if (stat /= 0) then
         error = 4
         return
      end if

      block
         integer :: i, j, k = 1

         do i = lb(1), ub(1)
            do j = lb(2), ub(2)
               if (i == j) then
                  ptr_b(k) = ptr_a(i, j)
                  k = k + 1
               end if
            end do
         end do
      end block
   end subroutine get_diagonal

   subroutine print_array(arr)
      implicit none

      integer, dimension(:), pointer, intent(in) :: arr
      integer :: i

      write (output_unit, *) (arr(i), i = 1, size(arr))
   end subroutine print_array
end program get_diagonal_elements
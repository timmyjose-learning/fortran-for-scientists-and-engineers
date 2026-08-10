program linked_list
   use, intrinsic :: iso_fortran_env, only: input_unit, output_unit, error_unit, iostat_end
   implicit none

   type :: node_t
      real :: value
      type(node_t), pointer :: next
   end type node_t

   call run_app()

contains
   function make_node(value) result(node)
      implicit none

      real, intent(in) :: value
      type(node_t), pointer :: node
      integer :: stat
      character(len=512) :: errmsg

      allocate(node, stat=stat, errmsg=errmsg)
      if (stat /= 0) then
         write (error_unit, *) 'Failed to allocate: ' // errmsg
         error stop
      end if

      node%value = value
      node%next => null()
   end function make_node

   subroutine push_back(list, value)
      implicit none

      type(node_t), pointer, intent(inout) :: list
      real, intent(in) :: value
      type(node_t), pointer :: new_node => null(), prev, curr

      new_node => make_node(value)

      if (.not. associated(list)) then
         list => new_node
      else
         prev => null()
         curr => list

         do while (associated(curr))
            prev => curr
            curr => curr%next
         end do

         prev%next => new_node
      end if
   end subroutine push_back

   subroutine print_linked_list(list)
      implicit none

      type(node_t), pointer, intent(in) :: list
      type(node_t), pointer :: curr

      curr => list
      do while (associated(curr))
         write (output_unit, '(F8.3)', advance='no') curr%value
         curr => curr%next
      end do
      write (output_unit, '(/)')
   end subroutine print_linked_list

   subroutine destroy_linked_list(list)
      implicit none

      type(node_t), pointer, intent(inout) :: list
      type(node_t), pointer :: curr, next
      integer :: stat

      curr => list

      do while (associated(curr))
         next => curr%next
         deallocate(curr, stat=stat)
         curr => next
      end do
   end subroutine destroy_linked_list

   subroutine run_app()
      implicit none

      type(node_t), pointer :: head => null()
      character(len=512) :: filename
      integer :: iostat
      character(len=512) :: iomsg

      integer :: unit
      character(len=1024) :: line

      write (output_unit, *) 'Enter the filename'
      read (input_unit, *) filename

      open(newunit=unit, file=filename, status='old', action='read', iostat=iostat, iomsg=iomsg)

      if (iostat /= 0) then
         write (error_unit, *) 'Error opening file: ' // iomsg
         error stop
      end if

      do
         read (unit, *, iostat=iostat, iomsg=iomsg) line
         if (iostat /= 0)  exit

         block
            real :: value
            read (line, '(F8.3)', iostat=iostat, iomsg=iomsg) value

            if (iostat /= 0) then
               write (error_unit, *) 'Invalid record: ' // iomsg
            else
               call push_back(head, value)
            end if
         end block
      end do

      if (iostat /= iostat_end) then
         close(unit)
         write (error_unit, *) 'Error while reading file: ' // iomsg
         error stop
      end if

      close(unit)

      call print_linked_list(head)
      call destroy_linked_list(head)
   end subroutine run_app
end program linked_list
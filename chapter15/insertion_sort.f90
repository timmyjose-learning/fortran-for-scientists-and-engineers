program insertion_sort
   use, intrinsic :: iso_fortran_env, only: input_unit, output_unit, error_unit, iostat_end
   implicit none

   type :: node_t
      integer :: value = 0.0
      type(node_t), pointer :: next => null()
   end type node_t

   call run_app()

contains
   subroutine run_app()
      implicit none

      type(node_t), pointer :: head => null()

      character(len=512) :: filename
      integer :: unit
      integer :: iostat
      character(len=512) :: iomsg
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
         if (iostat /= 0) exit

         block
            integer :: value

            read (line, *, iostat=iostat, iomsg=iomsg) value
            if (iostat /= 0) then
               write (error_unit, *) 'Invalid value: ' // line
               cycle
            else
               ! Sort while inserting into the linked list
               sort: block
                  type(node_t), pointer :: node => null(), prev => null(), curr => null()

                  node => make_node(value)

                  if (.not. associated(head)) then
                     head => node
                  else if (node%value <= head%value) then
                     node%next => head
                     head => node
                  else
                     curr => head

                     do while (associated(curr))
                        if (curr%value < node%value) then
                           prev => curr
                           curr => curr%next
                        else
                           exit
                        end if
                     end do

                     if (.not. associated(curr)) then
                        prev%next => node
                     else
                        prev%next => node
                        node%next => curr
                     end if
                  end if
               end block sort
            end if
         end block
      end do

      if (iostat /= iostat_end) then
         close(unit)
         write (error_unit, *) 'Error while reading file: ' // iomsg
         error stop
      end if

      close(unit)

      call print_list(head)
      call destroy_list(head)
   end subroutine run_app

   function make_node(value) result(node)
      implicit none

      integer, intent(in) :: value
      type(node_t), pointer :: node
      integer :: stat
      character(len=512) :: errmsg

      allocate(node, stat=stat, errmsg=errmsg)
      if (stat /= 0) then
         write (error_unit, *) '[push_back] Error: ' // errmsg
         error stop
      end if

      node%value = value
      node%next => null()
   end function make_node

   subroutine destroy_list(head)
      implicit none

      type(node_t), pointer, intent(inout) :: head
      type(node_t), pointer :: curr, next
      integer :: stat

      curr => head
      next => null()

      do while (associated(curr))
         next => curr%next
         deallocate(curr, stat=stat)
         curr => next
      end do
   end subroutine destroy_list


   subroutine print_list(head)
      implicit none

      type(node_t), pointer, intent(in) :: head
      type(node_t), pointer :: curr

      curr => head

      do while (associated(curr))
         write (output_unit, '(I5)', advance='no') curr%value
         curr => curr%next
      end do
      write (output_unit, '(/)')
   end subroutine print_list
end program insertion_sort
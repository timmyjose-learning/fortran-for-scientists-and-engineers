module bst_m
   use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
   implicit none
   private

   type :: node_t
      private
      integer :: value
      type(node_t), pointer :: left => null()
      type(node_t), pointer :: right => null()
   end type node_T

   type :: bst_t
      type(node_t), pointer :: root => null()

   contains
      procedure, pass(this) :: insert => insert_node
      procedure, pass(this) :: preorder => display_preorder
      procedure, pass(this) :: inorder => display_inorder
      procedure, pass(this) :: postorder => display_postorder
      procedure, pass(this) :: destroy => destroy_tree
   end type bst_t

   public :: bst_t

contains
   function make_node(value) result(node)
      implicit none

      integer, intent(in) :: value
      type(node_t), pointer :: node
      integer :: stat
      character(len=512) :: errmsg

      allocate(node, stat=stat, errmsg=errmsg)
      if (stat /= 0) then
         write (error_unit, *) 'Node allocation failed: ' // errmsg
         error stop
      end if

      node%value = value
      nullify(node%left)
      nullify(node%right)
   end function make_node

   subroutine destroy_tree(this)
      implicit none

      class(bst_t), intent(inout) :: this

      call destroy_tree_helper(this%root)
   end subroutine destroy_tree

   recursive subroutine destroy_tree_helper(root)
      implicit none

      type(node_t), pointer, intent(inout) :: root
      integer :: stat
      character(len=512) :: errmsg

      if (.not. associated(root)) then
         return
      end if

      call destroy_tree_helper(root%left)
      call destroy_tree_helper(root%right)
      deallocate(root, stat=stat, errmsg=errmsg)

      if (stat /= 0) then
         write (error_unit, *) 'Error while deallocating node: ' // errmsg
      end if
   end subroutine destroy_tree_helper

   subroutine insert_node(this, value)
      implicit none

      class(bst_t), intent(inout) :: this
      integer, intent(in) :: value

      call insert_node_helper(this%root, value)
   end subroutine insert_node

   recursive subroutine insert_node_helper(root, value)
      implicit none

      type(node_t), pointer, intent(inout) :: root
      integer, intent(in) :: value

      if (.not. associated(root)) then
         root => make_node(value)
      else if (value <= root%value) then
         call insert_node_helper(root%left, value)
      else
         call insert_node_helper(root%right, value)
      end if
   end subroutine insert_node_helper

   subroutine display_preorder(this)
      implicit none

      class(bst_t), intent(in) :: this

      call display_preorder_helper(this%root)
   end subroutine display_preorder

   recursive subroutine display_preorder_helper(root)
      implicit none

      type(node_t), pointer, intent(in) :: root

      if (.not. associated(root)) then
         return
      end if

      write (output_unit, '(I5)') root%value
      call display_preorder_helper(root%left)
      call display_preorder_helper(root%right)
   end subroutine display_preorder_helper

   subroutine display_inorder(this)
      implicit none

      class(bst_t), intent(in) :: this

      call display_inorder_helper(this%root)
   end subroutine display_inorder

   recursive subroutine display_inorder_helper(root)
      implicit none

      type(node_t), pointer, intent(in) :: root

      if (.not. associated(root)) then
         return
      end if

      call display_inorder_helper(root%left)
      write (output_unit, '(I5)') root%value
      call display_inorder_helper(root%right)
   end subroutine display_inorder_helper

   subroutine display_postorder(this)
      implicit none

      class(bst_t), intent(in) :: this

      call display_postorder_helper(this%root)
   end subroutine display_postorder

   recursive subroutine display_postorder_helper(root)
      implicit none

      type(node_t), pointer,  intent(in) :: root

      if (.not. associated(root)) then
         return
      end if

      call display_postorder_helper(root%left)
      call display_postorder_helper(root%right)
      write (output_unit, '(I5)') root%value
   end subroutine display_postorder_helper
end module bst_m

program bst_demo
   use, intrinsic :: iso_fortran_env, only: input_unit, output_unit, error_unit
   use bst_m, only: bst_t
   implicit none

   call run_app()

contains
   function get_number() result(num)
      implicit none

      integer :: num
      integer :: iostat
      character(len=512) :: iomsg

      write (output_unit, *) 'Enter a number'
      read (input_unit, *, iostat=iostat, iomsg=iomsg) num

      if (iostat /= 0) then
         write (error_unit, *) 'Error: ' // iomsg
         error stop
      end if
   end function get_number

   subroutine run_app()
      implicit none

      type(bst_t) :: tree
      integer :: choice
      integer :: number

      do
         write (output_unit, '("Enter your choice: ",/,6(A,/))') '1. Insert', &
            '2. Display pre-order', '3. Display in-order', &
            '4. Display post-order', '5. Quit'

         read (input_unit, *) choice

         select case(choice)
          case (1)
            number = get_number()
            call tree%insert(number)
          case (2)
            call tree%preorder()
          case (3)
            call tree%inorder()
          case (4)
            call tree%postorder()
          case (5)
            call tree%destroy()
            exit
          case default
            write (error_Unit, *) 'Invalid option. Try again'
            cycle
         end select
      end do
   end subroutine run_app
end program bst_demo
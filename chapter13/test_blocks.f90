program test_blocks
   implicit none

   integer :: i = 1, j = 2, k = 3

   write (*, *) 'Before the blocks: ', i, j, k

   ! a new lexical block/scope
   test_block : block
      integer :: j

      do j = 1, 10
         write (*, *) i, j, k
         if (j > 2) exit
      end do
   end block test_block

   write (*, *) 'After the block: ', i, j, k
end program test_blocks
program scratch_file
   implicit none

   integer :: iostat
   character(len=256) :: iomsg
   real :: sample
   integer :: nrec = 0, rec
   integer :: unit
   integer :: idx

   write (*, *) 'Enter positive or zero values.'
   write (*, *) 'A negative value terminates input.'

   open (newunit=unit, status='scratch', iostat=iostat, iomsg=iomsg)

   if (iostat /= 0) then
      write (*, *) 'Error opening scratch file: ', iomsg
      stop
   end if

   do
      nrec = nrec + 1
      write (*, *) 'Enter sample ', nrec
      read (*, *) sample

      if (sample < 0.0) then
         exit
      end if

      write (unit, *) sample
   end do

   write (*, *) 'Which record do you want to see? (1 - ', nrec - 1, ' )'
   read (*, *) rec

   if (rec < 1 .or. rec > nrec) then
      write (*, *) 'Illegal record number ', rec
   else
      rewind (unit=unit)

      do idx = 1, rec
         read (unit, *) sample
      end do

      write (*, '(A20, A5, I5, F8.3)') 'The value of record ', ' is ', rec, sample
   end if

   close (unit)
end program scratch_file

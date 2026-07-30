program complex_numbers_demo
   implicit none

   complex :: c1 = (0.0, 0.0), c2 = (0.0, 0.0)

   write (*, *) 'Enter the first complex number'
   read (*,*) c1 ! Enter as '(3.0, 4.0)'

   write (*, *) 'Enter the second complex number'
   read (*,*) c2

   write (*, *) 'Sum = ', c1 + c2
   write (*, *) 'Difference = ', c1 - c2
   write (*, *) 'Produce = ', c1 * c2
   write (*, *) 'Quotient = ', c1 / c2
end program complex_numbers_demo
program ex_2_5
   implicit none

   real, parameter :: LAMBDA = 0.00012097
   real :: percent, ratio, age

   write (*, *) 'Enter the percentage of C-14 left'
   read (*, *) percent

   ratio = percent/100.0
   age = (-1.0/LAMBDA)*log(ratio)

   write (*, *) 'Age of the sample = ', age, ' years'
end program ex_2_5

program ex_2_3
   implicit none

   real, parameter :: ABSOLUTE_ZERO = 273.15
   real :: fahrenheit, kelvin

   write (*, *) 'Enter the temperatur in Fahrenheit'
   read (*, *) fahrenheit

   kelvin = (5.0/9.0)*(fahrenheit - 32.0) + ABSOLUTE_ZERO
   write (*, *) 'Temperature in Kelvin = ', kelvin
end program ex_2_3

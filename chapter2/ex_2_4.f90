program ex_2_4
   implicit none

   real, parameter :: PI = 3.141593, DEG_2_RAD = PI/180.0

   real :: volts, impedance, theta
   real :: amps, p, q, s, pf

   write (*, *) 'Enter the voltage'
   read (*, *) volts

   write (*, *) 'Enter the impedance and the angle in degrees'
   read (*, *) impedance, theta

   amps = volts/impedance
   p = volts*amps*cos(theta*DEG_2_RAD)
   q = volts*amps*sin(theta*DEG_2_RAD)
   s = volts*amps
   pf = cos(theta*DEG_2_RAD)

   write (*, *) 'Voltage = ', volts, ' Volts'
   write (*, *) 'Impedance = ', impedance, ' ohms', ' at ', theta, ' degrees'
   write (*, *) 'Rms current = ', amps, ' Amps'
   write (*, *) 'Real power = ', p, ' Watts'
   write (*, *) 'Reactive power = ', q, ' Watts'
   write (*, *) 'Apparent power = ', s, ' Watts'
   write (*, *) 'Power Factor = ', pf
end program ex_2_4

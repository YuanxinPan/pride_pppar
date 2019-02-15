real*8 function wpress(rh, t)
!
!     Returns the partial pressure of water vapor, in mbar
!
!     INPUT:
!       RH     Relative humidity (0-1)
!       T      Temperature, deg C
!
  real*8 rh, t
!
  wpress = rh*6.11d0*10.d0**(7.5d0*t/(t + 2.373d2))
!
  return
end

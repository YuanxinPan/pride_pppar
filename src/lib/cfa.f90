!
!! cfa.f90
!!
!!    Copyright (C) 2018 by J.Geng
!!
!!    This program is free software: you can redistribute it and/or modify
!!    it under the terms of the GNU General Public License (version 3) as
!!    published by the Free Software Foundation.
!!
!!    This program is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!    GNU General Public License (version 3) for more details.
!!
!!    You should have received a copy of the GNU General Public License
!!    along with this program.  If not, see <https://www.gnu.org/licenses/>.
real*8 function cfa(p, t, wetvar, h2otyp, phi, h, elev)
!
!     Calculates the CfA-2.2 mapping function (Davis et al., Radio
!     Science 20, 1593-1607, 1985)
!
!     INPUT:    p      Pressure, mbar
!               t      Temperature, deg C
!               wetvar Water vapor variable, defined below
!               h2otyp H2OTYP = 'R' --> WETVAR is relative humidity (0-1)
!                      H2OTYP = 'D' --> WETVAR is dew point temperature (deg C)
!               phi    Geocentric latitude (radians)
!               h      Elevation above ellipsoid, km
!               elev   Elevation angle, radians
!
  implicit none
  real*8 p, t, wetvar, phi, h, elev, wpress, e
  real*8 a0, a1, a2, a3, a5
  real*8 b0, b1, b2, b3, b5
  real*8 c0
  real*8 a, b, c
  real*8 sinel, tanel
!
  character*1 h2otyp, upper_string
!
  data a0/0.001185D+00/
  data a1/0.6071D-04/
  data a2/-0.1471D-03/
  data a3/0.3072D-02/
!      data a4 /  0.1965D-01 /
  data a5/-0.5645D-02/
!
  data b0/0.001144D+00/
  data b1/0.1164D-04/
  data b2/0.2795D-03/
  data b3/0.3109D-02/
!      data b4 /  0.3038D-01 /
  data b5/-0.1217D-01/
!
  data c0/-0.009D+00/
!
!.... Get partial pressure of water vapor
  if (h2otyp .eq. upper_string('R')) then
    e = wpress(wetvar, t)
  else
    e = wpress(1.0D0, wetvar)
  end if
!
!.... The following expressions assume that the temperature lapse rate
!     is its nominal value of -6.5 K/km.  The tropopause height is assumed
!     to be the nominal sea-level value of 11.231 km less the height
!     of the station.
!

!** temporary fix for elev=90. case

  if (elev .eq. 90.d0) then
    cfa = 1.d0
    goto 999
  endif

!.... Calculate sine and tangent of elevation
  sinel = sin(elev)
!** temporary fix for elev=90. case
  if (dabs(sinel - 1.d0) .lt. 1.d-10) then
    cfa = 1.d0
    goto 999
  endif
  tanel = sinel/cos(elev)
!
!.... Caclulate the a, b, and c functions
  a = a0*(1.0D+00 + a1*(p - 1000.0) &
          + a2*e &
          + a3*(t - 20.0) &
          + a5*(-h))
!
  b = b0*(1.0D+00 + b1*(p - 1000.0) &
          + b2*e &
          + b3*(t - 20.0) &
          + b5*(-h))
!
  c = c0
!
!.... Calculate mapping function
  cfa = 1.0D0/(sinel + a/(tanel + b/(sinel + c)))
!
999 return
end

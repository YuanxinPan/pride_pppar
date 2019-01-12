!
!! saaszd.f90
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
real*8 function saaszd(p,t,wetvar,h2otyp,phi,h)
!
!     Calculates Saastamoinen zenith delay, in meters.
!
!     INPUT:
!       P        Total pressure, mbars
!       T        Temperature, deg C
!       WETVAR   Water vapor variable, defined by H2OTYP
!       H2OTYP   Defines WETVAR.  H2OTYP = 'R' indicates that WETVAR is relativ
!                humidity (0-1).  H2OTYP = 'D' indicates that WETVAR is the dew
!                point temperature (deg C).
!       PHI      Geocentric latitude, radians
!       H        Elevation above the geoid, km
!
real*8 p,t,wetvar,phi,h,e,ffun,wpress,tk
!
character*1 h2otyp,upper_string
!
!.... Calculate the partial pressure of water vapor, in mbars
if(h2otyp.eq.upper_string('R')) then
  e=wpress(wetvar,t)
else
  e=wpress(1.d0,wetvar)
endif
!
!! Temperature in Kelvin
tk=t+273.15d0
!
!! Zenith delay, meters
saaszd=0.2277d-2*(p+(0.1255d4/tk+0.5d-1)*e)/ffun(phi,h)
return
end

!
!! saaszd.f90
!!
!!    Copyright (C) 2018 by Wuhan University
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
!!
!!   PURPOSE: Compute the Saastamoinen zenith delay (meter)
!!
!!   AUTHOR : Shaoming Xin    jsx_miracle@whu.edu.cn
!!
!!   VERSION: ver 1.00        Jan-25-2019
!!
!!   DATE   : Jan-25, 2019
!!
!!   INPUT  : pr    (pressure(mbar))
!!            temp  (temperature)
!!            wvv   (Water vapor variable)
!!            rdtyp (R:relative humdity, D:dew point temperature)
!!            phi   (Geocentric latitude)
!!            elev  (Elevation above the geoid)

real*8 function saaszd(pr, temp, wvv, rdtyp, phi, elev)
implicit none
real*8 pr, temp, wvv, phi, elev, wp
character*1 rdtyp
!! function
real*8 wpress,ffun
character*1 upper_string
!
if (rdtyp .eq. upper_string('R')) then
  wp = wpress(wvv, temp)
else
  wp = wpress(1.d0, wvv)
endif
!! zenith delay
saaszd = 0.2277d-2*((0.1255d4/(temp + 273.15d0) + 0.5d-1)*wp + pr)/ffun(phi, elev)
return
end

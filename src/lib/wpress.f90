!
!! wpress.f90
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
real*8 function wpress(rh,t)
!
!     Returns the partial pressure of water vapor, in mbar
!
!     INPUT:
!       RH     Relative humidity (0-1)
!       T      Temperature, deg C
!
real*8 rh,t
!
wpress=rh*6.11d0*10.d0**(7.5d0*t/(t+2.373d2))
!
return
end

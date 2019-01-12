!
!! ffun.f90
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
real*8 function ffun(phi, h)
!
!     Computes the ellipsoidal/elevation-dependent variation of the
!     average acceleration of gravity from Saastamoinen model.
!
!     INPUT:
!       PHI    Geocentric latitude, radians
!       H      Elevation of site above geoid, km
!
  real*8 phi, h
!
  ffun = 1.d0 - 0.266d-2*dcos(2.d0*phi) - 0.28d-3*h
  return
end

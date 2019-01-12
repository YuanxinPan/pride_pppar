!
!! GMST2000.f90
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
!
REAL*8 FUNCTION GMST2000(UTA, UTB, TTA, TTB)
!+
!  - - - - - - - - -
!   G M S T 2 0 0 0
!  - - - - - - - - -
!
!  Greenwich Mean Sidereal Time (model consistent with IAU 2000
!  resolutions).
!
!  Annexe to IERS Conventions 2000, Chapter 5
!
!  Given:
!     UTA, UTB     d      UT1 date (JD = UTA+UTB)
!     TTA, TTB     d      TT date (JD = TTA+TTB)
!
!  The result is the Greenwich Mean Sidereal Time (radians), in the
!  range 0 to 2pi.
!
!  This revision:  2002 December 2
!
!-----------------------------------------------------------------------

  IMPLICIT NONE

  REAL*8 UTA, UTB, TTA, TTB

!  Arcseconds to radians
  REAL*8, PARAMETER :: DAS2R = 4.848136811095359935899141D-6

!  Reference epoch (J2000), JD
  REAL*8, PARAMETER :: DJ0 = 2451545D0

!  Days per Julian century
  REAL*8, PARAMETER :: DJC = 36525D0

  REAL*8, PARAMETER :: D2PI = 6.283185307179586476925287D0

  REAL*8 T

  REAL*8 ERA2000

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!  TT Julian centuries since J2000.0.
  T = ((TTA - DJ0) + TTB)/DJC

!  Greenwich Mean Sidereal Time, IAU 2000.
  GMST2000 = ERA2000(UTA, UTB) + (0.014506D0 + (4612.15739966D0 + (1.39667721D0 + &
                                                                   (-0.00009344D0 + 0.00001882D0*T)*T)*T)*T)*DAS2R
  GMST2000 = MOD(GMST2000, D2PI)
  IF (GMST2000 .LT. 0.D0) GMST2000 = GMST2000 + D2PI

  RETURN
END

!
!! SP2000.f90
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
REAL*8 FUNCTION SP2000(DATE1,DATE2)
!+
!  - - - - - - -
!   S P 2 0 0 0
!  - - - - - - -
!
!  The quantity s', positioning the Terrestrial Ephemeris Origin on the
!  equator of the Celestial Intermediate Pole.
!
!  Annex to IERS Conventions 2000, Chapter 5
!
!  Given:
!     DATE1,DATE2    d      TT date (JD = DATE1+DATE2)
!
!  Returned:
!     SP2000         d      the quantity s' in radians
!
!  This revision:  2002 November 25
!
!-----------------------------------------------------------------------
IMPLICIT NONE

REAL*8 DATE1, DATE2

!  Arcseconds to radians
REAL*8,PARAMETER :: DAS2R=4.848136811095359935899141D-6

!  Reference epoch (J2000), JD
REAL*8,PARAMETER :: DJ0=2451545D0

!  Days per Julian century
REAL*8,PARAMETER :: DJC=36525D0

!  Time since J2000, in Julian centuries
REAL*8 T

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!  Interval between fundamental epoch J2000.0 and current date (JC).
T=((DATE1-DJ0)+DATE2)/DJC

!  Approximate S'.
SP2000=-47D-6 * T * DAS2R

RETURN
END

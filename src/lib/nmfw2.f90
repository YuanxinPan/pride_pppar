!
!! NMFW2.f90
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
Subroutine NMFW2(latitude,elev,wmf)

!     Received from Arthur Niell May 17, 1996
!     --rwk 960517

! new aen 930517 Routine to compute the new wmf2.0 mapping function which
!                depends only on latitude.

integer*4 i

!   a,b,c       - the a,b,and c coefficients in the continued fraction
!                 form of Marini
!   beta        - intermediate term in calculation
!   gamma       - intermediate term in calculation
!   sine        - Sine of elevation angle
!   cose        - Cos of elevation angle
!   wmf(1)      - wet delay mapping function
!   wmf(2)      - d_wet_mapping_function/d_elevation
!   topcon      - Constant of top of mapping fuinction to ensure
!                 that value is 1.0000 at zenith

real*8 a,b,c, beta, cose, wmf(2), gamma, sine, topcon

!   latitude   - latitude (degrees)
!   l          - absolute latitude
!   dl         - incremental latitude from last lat_wmf
!   elev       - elevation (degrees)
!   dl,da,db,dc  - used for interpolation

real*8 lat_wmf(5), abc_w2p0(5,3)
real*8 dl, da, db, dc
real*8 latitude, l, elev, deg2rad

!   define parameters used for calculating coefficients.

data lat_wmf / 15.d0, 30.d0, 45.d0, 60.d0, 75.d0/

!   coefficients are from fits to raytraces of the standard atmospheres
!   for July for latitudes 15, 45, 60, and 75 degrees latitude and for
!   January for 30 degrees latitude (930517).

data abc_w2p0 / &
 5.8021897d-4,5.6794847d-4,5.8118019d-4,5.9727542d-4,6.1641693d-4, &
 1.4275268d-3,1.5138625d-3,1.4572752d-3,1.5007428d-3,1.7599082d-3, &
 4.3472961d-2,4.6729510d-2,4.3908931d-2,4.4626982d-2,5.4736038d-2/

deg2rad = 3.14159265d0/180.d0 

a=0.d0
b=0.d0
c=0.d0

l = abs(latitude)

!   Coefficients for the continued fraction expansion for each latitude.

!   for latitudes less than 15 degrees:

if (l .le. lat_wmf(1)) then
   a = abc_w2p0(1,1)
   b = abc_w2p0(1,2)
   c = abc_w2p0(1,3)
endif

!   for latitudes between 15 and 75  degrees:

do i = 1,4
    if (l .gt. lat_wmf(i) .and. l .le. lat_wmf(i+1)) then
       dl = (l-lat_wmf(i))/(lat_wmf(i+1)-lat_wmf(i))
       da  =   abc_w2p0(i+1,1)-abc_w2p0(i,1)
       a   =   abc_w2p0(i,1) + dl*da
!     write(*,'(" dl,da ,a  ",6e15.6)')
!    .            dl,da ,a

       db  =   abc_w2p0(i+1,2)-abc_w2p0(i,2)
       b   =   abc_w2p0(i,2) + dl*db
!     write(*,'(" dl,db ,b ",6e15.6)')
!    .            dl,db ,b

       dc  =   abc_w2p0(i+1,3)-abc_w2p0(i,3)
       c   =   abc_w2p0(i,3) + dl*dc
!     write(*,'(" dl,dc ,c ",6e15.6)')
!    .            dl,dc ,c

    endif
end do

!   for latitudes greater than 75 degrees:

if (l .ge. lat_wmf(5)) then
   a = abc_w2p0(5,1)
   b = abc_w2p0(5,2)
   c = abc_w2p0(5,3)
endif

!   Now the coefficients exist; calculate the mapping function, wmf(1),
!       and the change of mapping function with elevation,
!       dwmf/d_el =wmf(2).
!   To calculate the delay-rate correction, d_tau/dt:
!       d_tau/dt = d_tau_zen/dt * wmf(1) + tau_zen * dwmf/d_el * d_el/dt

sine  = sin( elev * deg2rad)
cose  = cos( elev * deg2rad)
beta  = b/( sine + c )
gamma = a/( sine + beta)
topcon = (1.d0 + a/(1.d0 + b/(1.d0 + c)))

!     write(*,'("sine, cose, beta, gamma, topcon = ", 5f10.5)')
!    .           sine, cose, beta, gamma, topcon

wmf(1) = topcon / ( sine + gamma )

wmf(2) = -topcon / ( sine + gamma )**2 * &
         ( cose - a/( sine + beta)**2 * cose * &
         ( 1.d0 - b/( sine + c )**2 ) )

!     write(*,'("wmf(1), wmf(2) = ", 2f10.4)') wmf(1), wmf(2)
!     write(*,'("wmf(1), wmf(2) = ", 2f10.4)') wmf

!aen   write out diagnostic info.
!     write(*,'("  elev, latitude, wmf2.0, dwmf/del = ",4f15.6)')
!    .             elev, latitude, wmf(1), wmf(2)

return
end


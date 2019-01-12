!
!! NMFH2P1.f90
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
subroutine NMFH2P1(doy,latitude,height,elev,hmf)
!     Received from Arthur Niell May 17, 1996, and modified to
!     include the correction terms for latitudes > 75 degrees.
!     --rwk 960517

!     Routine to compute the hydrostatic mapping function nhmf2 which
!     depends on DOY (day of year) and station position (latitude
!     and height above geoid).

! 931007 aen NEW nhmf2
! 951129 aen MOD nmfh2p1 Add derivative of height correction wrt elevation to hmf(2).
!                NOTE change in spelling of subroutine from nhmf to nmfh.

implicit none 
integer*4 i

!   a,b,c       - the a,b,and c coeffiecents in the continued fraction
!                 form of Marini
!   beta        - intermediate term in calculation
!   gamma       - intermediate term in calculation
!   sine        - sine of elevation angle
!   cose        - cos of elevation angle
!   hmf(1)      - delay mapping function
!   hmf(2)      - d_mapping_function/d_elevation (dhmf2/d_el)
!   topcon      - constant of top of mapping function to ensure
!                 that value is 1.0000 at zenith

real*8 a,b,c, beta, cose, hmf(2), gamma, sine, topcon

!   height     - height of site above geoid (meters)
!   hs_km      - Height of site in kms.
!   latitude   - latitude (degrees)
!   l          - absolute latitude
!   dl         - incremental latitude from last lat_hmf
!   elev       - elevation (degrees)
!   epoch      - if Julian date of observation is known for the observation,
!              - then epoch can be used to get day of year.
!              - (if epoch  is passed as argument, then un-comment the
!                 line converting epoch to doy.)
!   doy        - days since Dec 31
!   doy_atm    - doy for atmosphere relative to Jan 28.
!   doyr_atm   - doy_atm in radians;
!   cost       - cosine(day of year)
!   doy2rad    - convert doy to radians

real*8 doy, latitude, height, elev
real*8 hs_km, l, dl, doy_atm, doyr_atm, cost
real*8 doy2rad, deg2rad

!   lat_hmf     - latitudes at which coefficients are defined (5).
!   abc_avg     - continued fraction coefficients at latitudes lat_hmf
!   abc_amp     - amplitude of annual variation of abc_avg
!   daavg, daamp, etc - incremental values for interpolation
!   aavg,  aamp,  etc - average and amplitude at latitude

real*8 lat_hmf(5)
real*8 abc_avg(5,3), abc_amp(5,3)
real*8 daavg, daamp, dbavg, dbamp, dcavg, dcamp
real*8 aavg,  aamp,  bavg,  bamp,  cavg,  camp

!   a_ht, b_ht, c_ht - parameters for continued fraction for height corr'n.
!   dhcc_del    - derivative of height correction coefficient with elevation
!   dht_corr_del - derivative of height correction with elevation

real*8 a_ht, b_ht, c_ht, ht_corr_coef, ht_corr
real*8 dhcc_del, dht_corr_del

!   define parameters used for calculating coefficients.

data lat_hmf / 15.d0, 30.d0, 45.d0, 60.d0, 75.d0/

data abc_avg / &
1.2769934d-3,1.2683230d-3,1.2465397d-3,1.2196049d-3,1.2045996d-3,&
2.9153695d-3,2.9152299d-3,2.9288445d-3,2.9022565d-3,2.9024912d-3,&
62.610505d-3,62.837393d-3,63.721774d-3,63.824265d-3,64.258455d-3/

data abc_amp / &
  0.0,   1.2709626d-5, 2.6523662d-5, 3.4000452d-5, 4.1202191d-5,&
  0.0,   2.1414979d-5, 3.0160779d-5, 7.2562722d-5, 11.723375d-5,&
  0.0,   9.0128400d-5, 4.3497037d-5, 84.795348d-5, 170.37206d-5/

data a_ht / 2.53d-5/ &
     b_ht / 5.49d-3/ &
     c_ht / 1.14d-3/

!   conversions:

doy2rad = 2.d0*3.14159265d0/365.25d0
deg2rad = 3.14159265d0/180.d0

a=0.d0
b=0.d0
c=0.d0

!   convert height in meters to kilometers

hs_km  = height/1000.d0

!   If Julian date is used for epoch, then calculate day of year;
!      use 1980 Jan 0 as reference epoch.

!     doy = epoch - 2444238.5d0

! mod aen 930517 Use phase of 28 days (winter extremum corresponds to Jan 28)
!                based on least-square fit to
!                raytrace of radiosonde data for DRT, ELP, ALB, CHH, FAI,
!                MUN, and LIH.
!

doy_atm  = doy - 28.d0  

!   to account for the six month difference in seasons between hemispheres,
!   add 365.25/2 days to doy_atm if station is in the southern hemisphere.
       
l = abs(latitude)
if (latitude .lt. 0.d0) doy_atm = doy_atm + 365.25d0/2.d0

doyr_atm = doy_atm * doy2rad

!aen  debug
!     write(*,'("doy, doy_atm, doyr_atm = ", 3f15.6)') doy, doy_atm, doyr_atm
cost = cos(doyr_atm)

!   Coefficients for the continued fraction expansion for each latitude.

!   for latitudes less than 15 degrees:

if (l .le. lat_hmf(1)) then
   a = abc_avg(1,1)
   b = abc_avg(1,2)
   c = abc_avg(1,3)
endif

!   for latitudes between 15 and 75  degrees:

do i = 1,4
    if (l .gt. lat_hmf(i) .and. l .le. lat_hmf(i+1)) then
       dl = (l-lat_hmf(i))/(lat_hmf(i+1)-lat_hmf(i))
       daavg =   abc_avg(i+1,1)-abc_avg(i,1)
       daamp =   abc_amp(i+1,1)-abc_amp(i,1)
       aavg  =   abc_avg(i,1) + dl*daavg
       aamp  =   abc_amp(i,1) + dl*daamp
       a     = aavg - aamp*cost
!     write(*,'(" dl,daavg,daamp,aavg,aamp,a ",6e15.6)')
!    .            dl,daavg,daamp,aavg,aamp,a

       dbavg =   abc_avg(i+1,2)-abc_avg(i,2)
       dbamp =   abc_amp(i+1,2)-abc_amp(i,2)
       bavg  =   abc_avg(i,2) + dl*dbavg
       bamp  =   abc_amp(i,2) + dl*dbamp
       b     = bavg - bamp*cost
!     write(*,'(" dl,dbavg,dbamp,bavg,bamp,b ",6e15.6)')
!    .            dl,dbavg,dbamp,bavg,bamp,b

       dcavg =   abc_avg(i+1,3)-abc_avg(i,3)
       dcamp =   abc_amp(i+1,3)-abc_amp(i,3)
       cavg  =   abc_avg(i,3) + dl*dcavg
       camp  =   abc_amp(i,3) + dl*dcamp
       c     = cavg - camp*cost
!     write(*,'(" dl,dcavg,dcamp,cavg,camp,c ",6e15.6)')
!    .            dl,dcavg,dcamp,cavg,camp,c

    endif
end do

!   for latitudes greater than 75 degrees:

if (l .ge. lat_hmf(5)) then
  a = abc_avg(5,1) - abc_amp(5,1)*cost
  b = abc_avg(5,2) - abc_amp(5,2)*cost
  c = abc_avg(5,3) - abc_amp(5,3)*cost
endif

!   Now the coefficients exist; calculate for the sea level part
!   the mapping function, hmf(1), and the derivative wrt elevation
!   dhmf/d_el = hmf(2).

!   To get delay-rate correction d_tau/dt:
!      d_tau/dt = d_tau-zen/dt*hmf(1) + tau-zen*hmf(2)*d_el/dt
!      where  hmf(2)=dhmf/d_el

sine   = sin(elev * deg2rad)
cose   = cos(elev * deg2rad)
beta   = b/( sine + c )
gamma  = a/( sine + beta)
topcon = (1.d0 + a/(1.d0 + b/(1.d0 + c)))

hmf(1) =     topcon / ( sine + gamma )

hmf(2) =     -topcon*cose / ( sine + gamma )**2 * &
            ( 1.d0 - a/ ( sine + beta)**2 * &
            ( 1.d0 - b/ ( sine + c   )**2 ) )

!     write(*,'("sine, cose, beta, gamma, topcon = ", 5f10.5)')
!    .           sine, cose, beta, gamma, topcon
!     write(*,'("hmf(1), hmf(2) = ", 2f10.4)') hmf(1), hmf(2)
!     write(*,'("hmf(1), hmf(2) = ", 2f10.4)') hmf

!   Apply height correction to mapping function and derivative wrt elevation:
!
!      1) height correction coefficient is
!         1/sine(elev) - continued fraction(a_ht,b_ht,c_ht).
!      2) height correction is ht_corr_coef times height in km.
!      3) height correction to derivative wrt elevation is (derivative of
!         height correction coefficient wrt elevation)*height in km.

beta   = b_ht/( sine + c_ht )
gamma  = a_ht/( sine + beta)
topcon = (1.d0 + a_ht/(1.d0 + b_ht/(1.d0 + c_ht)))
ht_corr_coef = 1/sine - topcon/(sine + gamma)
ht_corr      = ht_corr_coef * hs_km
hmf(1)       = hmf(1)     + ht_corr

!    951129 The derivative of the height correction wrt elevation is added
!    to hmf(2) after Chris Jacobs pointed out the magnitude of the term.

dhcc_del   = -cose/sine**2 &
             +topcon*cose / ( sine + gamma)**2 * &
            ( 1.d0 - a_ht/ ( sine + beta)**2 *   &
            ( 1.d0 - b_ht/ ( sine + c_ht)**2) )
dht_corr_del = dhcc_del * hs_km
hmf(2)       = hmf(2)     + dht_corr_del

!     write(*,'("ht_corr_coef, ht_corr, hs_km, hmf(1) = ", 4f15.6)')
!    .           ht_corr_coef, ht_corr, hs_km, hmf(1)

return
end

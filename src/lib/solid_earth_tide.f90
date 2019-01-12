!
!! SOLID_EARTH_TIDE.f90
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
SUBROUTINE SOLID_EARTH_TIDE(XSTA,IJD,FHR,XSUN,XMON,DXTIDE)
!  
! PURPOSE    :  COMPUTATION OF TIDAL CORRECTIONS OF STATION DISPLACEMENTS  
!               CAUSED BY LUNAR AND SOLAR GRAVITATIONAL ATTRACTION  
!               (SEE IERS STANDARDS 1996)  
!         STEP 1 (HERE GENERAL DEGREE 2 AND 3 CORRECTIONS +  
!                CALL ST1IDIU + CALL ST1ISEM + CALL ST1L1) 
!         + STEP 2 (CALL STEP2DIU + CALL STEP2LON + CALL STEP2IDIU)  
! IT HAS BEEN DECIDED THAT THE STEP 3 NON-CORRECTION FOR PERMANENT TIDE 
! WOULD NOT BE APPLIED IN ORDER TO AVOID JUMP IN THE REFERENCE FRAME 
! (THIS STEP 3 MUST ADDED IN ORDER TO GET THE NON-TIDAL STATION POSITION  
! AND TO BE CONFORMED WITH THE IAG RESOLUTION.)  
!  
!    INPUT :  XSTA(I),I=1,2,3: GEOCENTRIC POSITION OF THE STATION (ITRF,
!                              CO-ROTATING FRAME) -- UNITS = METERS
!             XSUN(I),I=1,2,3: GEOC. POSITION OF THE SUN (ECEF FRAME) --
!                              UNITS = METERS
!             XMON(I),I=1,2,3: GEOC. POSITION OF THE MOON (ECEF FRAME) --
!                              UNITS = METERS
!             IYR : YEAR (UTC TIMESCALE)
!             IMONTH : MONTH (UTC TIMESCALE)
!             IDAY : DAY (UTC TIMESCALE)
!             FHR=hr+zmin/60.+sec/3600. : HR IN THE DAY 
!   OUTPUT :  DXTIDE(I),I=1,2,3: DISPLACEMENT VECTOR (GEOCENTRIC ITRF FRAME) --
!                                UNITS = METERS
!  
! SUBROUTINES CALLED  :  SPROD  
!                        ST1IDIU 
!                        ST1ISEM 
!                        ST1L1 
!                        STEP2DIU  
!                        STEP2LON 
!                        STEP2ILON  
!  
! AUTHOR IERS 1996 :  V. DEHANT, S. MATHEWS AND J. GIPSON
!    (TEST BETWEEN TWO SUBROUTINES) 
! AUTHOR IERS 2000 :  V. DEHANT AND S. MATHEWS
!    (TEST IN THE BERNESE PROGRAM BY C. BRUYNINX)
!  
! CREATED    :  96/03/23              LAST MODIFIED :  00/05/17 14:10  
! UPDATED    :2006/02/06  HEADER COMMENTS MODIFIED TO CLARIFY INPUT/OUTPUT
!                         UNITS AND SYSTEMS BY JIM RAY
! UPDATED    :2006/02/06  SUBROUTINE DUTC MODIFIED FOR LEAP SECOND ON
!                         2006.0 AND TO CORRECT do 5 i=1,87 from 84 to 87
!                         BY JIM RAY
! UPDATED    :2006/08/31  CORRECT DUTC FOR DATES AFTER 2007 (G. PETIT)
! UPDATED    :2007/06/20  MODIFIED SUBROUTINE DUTC TO CORRECT PAST MISTAKE (PROVIDED BY H. MANCHE) 
!                         CORRECT STEP2DIU LINE       DE=DATDI(8,J)*SINPHI*COS(THETAF+ZLA)+
!                                            TO       DE=DATDI(8,J)*SINPHI*COS(THETAF+ZLA)-
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DOUBLE PRECISION XSTA(3),XSUN(3),XMON(3),DXTIDE(3),XCORSTA(3)
DOUBLE PRECISION H20,L20,H3,L3,H2,L2
DOUBLE PRECISION mass_ratio_sun,mass_ratio_moon
!
! FUNCTION CALLED
!----------------
INTEGER*4 MODIFIED_JULDAY
REAL*8 TAIUTC
!  
! NOMINAL SECOND DEGREE AND THIRD DEGREE LOVE NUMBERS AND SHIDA NUMBERS  
! ---------------------------------------------------------------------  
DATA H20/0.6078D0/,L20/0.0847D0/,H3/0.292D0/,L3/0.015D0/
!  
! SCALAR PRODUCT OF STATION VECTOR WITH SUN/MOON VECTOR  
! -----------------------------------------------------  
CALL SPROD(XSTA,XSUN,SCS,RSTA,RSUN)  
CALL SPROD(XSTA,XMON,SCM,RSTA,RMON)  
SCSUN=SCS/RSTA/RSUN  
SCMON=SCM/RSTA/RMON
!   
! COMPUTATION OF NEW H2 AND L2  
! ----------------------------  
COSPHI=DSQRT(XSTA(1)**2+XSTA(2)**2)/RSTA
H2=H20-0.0006*(1.-3./2.*COSPHI**2)
L2=L20+0.0002*(1.-3./2.*COSPHI**2)  
!
! P2-TERM  
! -------  
P2SUN=3.*(H2/2.-L2)*SCSUN**2-H2/2.  
P2MON=3.*(H2/2.-L2)*SCMON**2-H2/2.  
!  
! P3-TERM  
! -------  
P3SUN=5./2.*(H3-3.*L3)*SCSUN**3+3./2.*(L3-H3)*SCSUN
P3MON=5./2.*(H3-3.*L3)*SCMON**3+3./2.*(L3-H3)*SCMON
!  
! TERM IN DIRECTION OF SUN/MOON VECTOR  
! ------------------------------------  
X2SUN=3.*L2*SCSUN  
X2MON=3.*L2*SCMON  
X3SUN=3.*L3/2.*(5.*SCSUN**2-1.)  
X3MON=3.*L3/2.*(5.*SCMON**2-1.)
!  
! FACTORS FOR SUN/MOON  
! --------------------  
MASS_RATIO_SUN=332945.943062d0
MASS_RATIO_MOON=0.012300034d0
RE =6378136.55d0
FAC2SUN=MASS_RATIO_SUN*RE*(RE/RSUN)**3
FAC2MON=MASS_RATIO_MOON*RE*(RE/RMON)**3
FAC3SUN=FAC2SUN*(RE/RSUN)
FAC3MON=FAC2MON*(RE/RMON)
!  
! TOTAL DISPLACEMENT  
! ------------------  
DO I=1,3  
  DXTIDE(I)=FAC2SUN*( X2SUN*XSUN(I)/RSUN + P2SUN*XSTA(I)/RSTA )+&
            FAC2MON*( X2MON*XMON(I)/RMON + P2MON*XSTA(I)/RSTA )+&  
            FAC3SUN*( X3SUN*XSUN(I)/RSUN + P3SUN*XSTA(I)/RSTA )+&   
            FAC3MON*( X3MON*XMON(I)/RMON + P3MON*XSTA(I)/RSTA )  
ENDDO  
DO I=1,3
  XCORSTA(I)=0.D0
ENDDO
!  
! CORRECTIONS FOR THE OUT-OF-PHASE PART OF LOVE NUMBERS (PART H_2^(0)I  
!            AND L_2^(0)I )  
! FIRST, FOR THE DIURNAL BAND       
CALL ST1IDIU(XSTA,XSUN,XMON,FAC2SUN,FAC2MON,XCORSTA)
DO I=1,3
  DXTIDE(I)=DXTIDE(I)+XCORSTA(I)  
ENDDO
!  
! SECOND, FOR THE SEMI-DIURNAL BAND       
! 
CALL ST1ISEM(XSTA,XSUN,XMON,FAC2SUN,FAC2MON,XCORSTA)
DO I=1,3
  DXTIDE(I)=DXTIDE(I)+XCORSTA(I)
ENDDO  
!  
! CORRECTIONS FOR THE LATITUDE DEPENDENCE OF LOVE NUMBERS (PART L^(1) )  
!   
CALL ST1L1(XSTA,XSUN,XMON,FAC2SUN,FAC2MON,XCORSTA)
DO I=1,3  
  DXTIDE(I)=DXTIDE(I)+XCORSTA(I)
ENDDO    
!  
! CONSIDER CORRECTIONS FOR STEP 2  
!  
! CORRECTIONS FOR THE DIURNAL BAND:  
! 
!  FIRST, WE NEED TO KNOW THE DATE CONVERTED IN JULIAN CENTURIES 
!        
!   1) CALL THE SUBROUTINE COMPUTING THE JULIAN DATE 
! 
!      EXPRESSION OF THE HOURS, MINUTES AND SECONDES IN FRACTION OF DAY        
! 
!      djuld=fjldy(iyr,imonth,iday,fhr)
DJULD=IJD+2400000.5d0+FHR/24.D0
! convert to centuries.
T=(DJULD-2451545.)/36525.
! 
!   2) CALL THE SUBROUTINE COMPUTING THE CORRECTION OF UTC TIME  
! 
!      CALL DUTC(IYR,IMONTH,IDAY,DTT)
!      fhr=fhr+dtt/3600.
FHR=FHR+(TAIUTC(IJD)+32.184D0)/3600.D0
!  
!  SECOND, WE CAN CALL THE SUBROUTINE STEP2DIU, FOR THE DIURNAL BAND CORRECTIONS,
!   (in-phase and out-of-phase frequency dependence):
!  
CALL STEP2DIU(XSTA,FHR,T,XCORSTA)  
DO I=1,3  
  DXTIDE(I)=DXTIDE(I)+XCORSTA(I)
ENDDO  
!  
!  CORRECTIONS FOR THE LONG-PERIOD BAND,
!   (in-phase and out-of-phase frequency dependence):  
!
CALL STEP2LON(XSTA,FHR,T,XCORSTA)
DO I=1,3  
  DXTIDE(I)=DXTIDE(I)+XCORSTA(I)
ENDDO  
!    
! CONSIDER CORRECTIONS FOR STEP 3  
!  
! UNCORRECT FOR THE PERMANENT TIDE  
!  
!      PI=3.141592654
!      SINPHI=XSTA(3)/RSTA  
!      COSPHI=dsqrt(XSTA(1)**2+XSTA(2)**2)/RSTA
!      COSLA=XSTA(1)/COSPHI/RSTA  
!      SINLA=XSTA(2)/COSPHI/RSTA  
!      DR=-DSQRT(5./4./PI)*H2*0.31460*(3./2.*SINPHI**2-0.5)
!      DN=-DSQRT(5./4./PI)*L2*0.31460*3.*COSPHI*SINPHI
!      DXTIDE(1)=DXTIDE(1)-DR*COSLA*COSPHI+DN*COSLA*SINPHI
!      DXTIDE(2)=DXTIDE(2)-DR*SINLA*COSPHI+DN*SINLA*SINPHI  
!      DXTIDE(3)=DXTIDE(3)-DR*SINPHI      -DN*COSPHI
!       
RETURN  
END  
!*********************************************************************************************
! 
SUBROUTINE SPROD(X,Y,SCAL,R1,R2) 
! 
!  COMPUTATION OF THE SCALAR-PRODUCT OF TWO VECTORS AND THEIR NORMS 
! 
!  INPUT :  X(I),I=1,2,3: COMPONENTS OF VECTOR X 
!           Y(I),I=1,2,3: COMPONENTS OF VECTOR Y 
!  OUTPUT :  SCAL: SCALAR PRODUCT OF X AND Y 
!            R1,R2  : LENGTHS OF THE TWO VECTORS X AND Y 
! 
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DOUBLE PRECISION X(3),Y(3)
R1=DSQRT(X(1)**2+X(2)**2+X(3)**2) 
R2=DSQRT(Y(1)**2+Y(2)**2+Y(3)**2) 
SCAL=X(1)*Y(1)+X(2)*Y(2)+X(3)*Y(3) 
RETURN 
END 
! 
!-------------------------------------------------------------------------
!
SUBROUTINE ST1L1(XSTA,XSUN,XMON,FAC2SUN,FAC2MON,XCORSTA)
!  
! THIS SUBROUTINE GIVES THE CORRECTIONS INDUCED BY THE LATITUDE DEPENDENCE  
! GIVEN BY L^(1) IN MAHTEWS ET AL (1991)  
!  
!       INPUT : XSTA,XSUN,XMON,FAC3SUN,FAC3MON  
!      OUTPUT : XCORSTA  
!  
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
! compute the norm of a vector
DOUBLE PRECISION DOT
DIMENSION XSTA(3),XSUN(3),XMON(3),XCORSTA(3)  
DOUBLE PRECISION L1,L1D,L1SD
DATA L1D/0.0012d0/,L1SD/0.0024d0/
RSTA=DSQRT(DOT(3,XSTA,XSTA))
SINPHI=XSTA(3)/RSTA  
COSPHI=DSQRT(XSTA(1)**2+XSTA(2)**2)/RSTA  
SINLA=XSTA(2)/COSPHI/RSTA  
COSLA=XSTA(1)/COSPHI/RSTA  
RMON=DSQRT(DOT(3,XMON,XMON))
RSUN=DSQRT(DOT(3,XSUN,XSUN))
!
! FOR THE DIURNAL BAND  
!  
L1=L1D  
DNSUN=-L1*SINPHI**2*FAC2SUN*XSUN(3)*(XSUN(1)*COSLA+XSUN(2)*SINLA)/RSUN**2
DNMON=-L1*SINPHI**2*FAC2MON*XMON(3)*(XMON(1)*COSLA+XMON(2)*SINLA)/RMON**2
DESUN=L1*SINPHI*(COSPHI**2-SINPHI**2)*FAC2SUN*XSUN(3)*(XSUN(1)*SINLA-XSUN(2)*COSLA)/RSUN**2
DEMON=L1*SINPHI*(COSPHI**2-SINPHI**2)*FAC2MON*XMON(3)*(XMON(1)*SINLA-XMON(2)*COSLA)/RMON**2
DE=3.*(DESUN+DEMON)  
DN=3.*(DNSUN+DNMON)  
XCORSTA(1)=-DE*SINLA-DN*SINPHI*COSLA  
XCORSTA(2)=DE*COSLA-DN*SINPHI*SINLA  
XCORSTA(3)=DN*COSPHI  
!   
! FOR THE SEMI-DIURNAL BAND  
!  
L1=L1SD  
COSTWOLA=COSLA**2-SINLA**2  
SINTWOLA=2.*COSLA*SINLA  
DNSUN=-L1/2.*SINPHI*COSPHI*FAC2SUN*((XSUN(1)**2-XSUN(2)**2)*COSTWOLA+2.*XSUN(1)*XSUN(2)*SINTWOLA)/RSUN**2
DNMON=-L1/2.*SINPHI*COSPHI*FAC2MON*((XMON(1)**2-XMON(2)**2)*COSTWOLA+2.*XMON(1)*XMON(2)*SINTWOLA)/RMON**2
DESUN=-L1/2.*SINPHI**2*COSPHI*FAC2SUN*((XSUN(1)**2-XSUN(2)**2)*SINTWOLA-2.*XSUN(1)*XSUN(2)*COSTWOLA)/RSUN**2
DEMON=-L1/2.*SINPHI**2*COSPHI*FAC2MON*((XMON(1)**2-XMON(2)**2)*SINTWOLA-2.*XMON(1)*XMON(2)*COSTWOLA)/RMON**2
DE=3.*(DESUN+DEMON)  
DN=3.*(DNSUN+DNMON)  
XCORSTA(1)=XCORSTA(1)-DE*SINLA-DN*SINPHI*COSLA  
XCORSTA(2)=XCORSTA(2)+DE*COSLA-DN*SINPHI*SINLA  
XCORSTA(3)=XCORSTA(3)+DN*COSPHI  
RETURN  
END  
!*************************************************************************
!     Last change:  VD   17 May 00   1:20 pm
!  THESE ARE THE SUBROUTINES FOR THE STEP2 OF THE TIDAL CORRECTIONS. 
!  THEY ARE CALLED TO ACCOUNT FOR THE FREQUENCY DEPENDENCE  
!  OF THE LOVE NUMBERS. 
! 
SUBROUTINE STEP2DIU(XSTA,FHR,T,XCORSTA)  
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DOUBLE PRECISION XSTA(3),XCORSTA(3),DATDI(9,31)
DOUBLE PRECISION deg2rad
DATA deg2rad/0.0174532925d0/
DATA ((DATDI(i,j),i=1,9),j=1,31)/ & 
 -3., 0., 2., 0., 0.,-0.01, 0.0 , 0.0 , 0.0,   &
 -3., 2., 0., 0., 0.,-0.01, 0.0 , 0.0 , 0.0,   &
 -2., 0., 1.,-1., 0.,-0.02, 0.0 , 0.0 , 0.0,   &
 -2., 0., 1., 0., 0.,-0.08, 0.0 ,-0.01, 0.01,  &
 -2., 2.,-1., 0., 0.,-0.02, 0.0 , 0.0 , 0.0,   &
 -1., 0., 0.,-1., 0.,-0.10, 0.0 , 0.0 , 0.0,   &
 -1., 0., 0., 0., 0.,-0.51, 0.0 ,-0.02, 0.03,  &
 -1., 2., 0., 0., 0., 0.01, 0.0 , 0.0 , 0.0,   &
  0.,-2., 1., 0., 0., 0.01, 0.0 , 0.0 , 0.0,   &
  0., 0.,-1., 0., 0., 0.02, 0.0 , 0.0 , 0.0,   &
  0., 0., 1., 0., 0., 0.06, 0.0 , 0.0 , 0.0,   &
  0., 0., 1., 1., 0., 0.01, 0.0 , 0.0 , 0.0,   &
  0., 2.,-1., 0., 0., 0.01, 0.0 , 0.0 , 0.0,   &
  1.,-3., 0., 0., 1.,-0.06, 0.0 , 0.0 , 0.0,   &
  1.,-2., 0.,-1., 0., 0.01, 0.0 , 0.0 , 0.0,   &
  1.,-2., 0., 0., 0.,-1.23,-0.07, 0.06, 0.01,  &
  1.,-1., 0., 0.,-1., 0.02, 0.0 , 0.0 , 0.0,   &
  1.,-1., 0., 0., 1., 0.04, 0.0 , 0.0 , 0.0,   &
  1., 0., 0.,-1., 0.,-0.22, 0.01, 0.01, 0.0,   &
  1., 0., 0., 0., 0.,12.00,-0.80,-0.67,-0.03,  &
  1., 0., 0., 1., 0., 1.73,-0.12,-0.10, 0.0,   &
  1., 0., 0., 2., 0.,-0.04, 0.0 , 0.0 , 0.0,   &
  1., 1., 0., 0.,-1.,-0.50,-0.01, 0.03, 0.0,   &
  1., 1., 0., 0., 1., 0.01, 0.0 , 0.0 , 0.0,   &
  0., 1., 0., 1.,-1.,-0.01, 0.0 , 0.0 , 0.0,   &
  1., 2.,-2., 0., 0.,-0.01, 0.0 , 0.0 , 0.0,   &
  1., 2., 0., 0., 0.,-0.11, 0.01, 0.01, 0.0,   &
  2.,-2., 1., 0., 0.,-0.01, 0.0 , 0.0 , 0.0,   &
  2., 0.,-1., 0., 0.,-0.02, 0.0 , 0.0 , 0.0,   &
  3., 0., 0., 0., 0., 0.0 , 0.0 , 0.0 , 0.0,   &
  3., 0., 0., 1., 0., 0.0 , 0.0 , 0.0 , 0.0/
S=218.31664563D0+481267.88194D0*T-0.0014663889D0*T**2+0.00000185139D0*T**3 
TAU=fhr*15.D0+280.4606184D0+36000.7700536D0*T+0.00038793D0*T**2-0.0000000258D0*T**3-S 
PR=1.396971278*T+0.000308889*T**2+0.000000021*T**3+0.000000007*T**4 
S=S+PR 
H=280.46645D0+36000.7697489D0*T+0.00030322222D0*T**2+0.000000020*T**3-0.00000000654*T**4 
P=83.35324312D0+4069.01363525D0*T-0.01032172222D0*T**2-0.0000124991D0*T**3+0.00000005263D0*T**4
ZNS=234.95544499D0 +1934.13626197D0*T-0.00207561111D0*T**2-0.00000213944D0*T**3+0.00000001650D0*T**4
PS=282.93734098D0+1.71945766667D0*T+0.00045688889D0*T**2-0.00000001778D0*T**3-0.00000000334D0*T**4 
! Reduce angles to between 0 and 360.
s=  dmod(s,360.d0)
tau=dmod(tau,360.d0)
h=  dmod(h,360.d0)
p=  dmod(p,360.d0)
zns=dmod(zns,360.d0)
ps=dmod(ps,360.d0)

RSTA=DSQRT(XSTA(1)**2+XSTA(2)**2+XSTA(3)**2)  
SINPHI=XSTA(3)/RSTA  
COSPHI=DSQRT(XSTA(1)**2+XSTA(2)**2)/RSTA  

COSLA=XSTA(1)/COSPHI/RSTA
SINLA=XSTA(2)/COSPHI/RSTA
ZLA = DATAN2(XSTA(2),XSTA(1))
DO I=1,3  
  XCORSTA(I)=0.
ENDDO
DO J=1,31
  THETAF=(TAU+DATDI(1,J)*S+DATDI(2,J)*H+DATDI(3,J)*P+DATDI(4,J)*ZNS+DATDI(5,J)*PS)*deg2rad
  DR=DATDI(6,J)*2.*SINPHI*COSPHI*SIN(THETAF+ZLA)+DATDI(7,J)*2.*SINPHI*COSPHI*COS(THETAF+ZLA)
  DN=DATDI(8,J)*(COSPHI**2-SINPHI**2)*SIN(THETAF+ZLA)+DATDI(9,J)*(COSPHI**2-SINPHI**2)*COS(THETAF+ZLA)
!  DE=DATDI(8,J)*SINPHI*COS(THETAF+ZLA)+
!  Modified 20 June 2007
  DE=DATDI(8,J)*SINPHI*COS(THETAF+ZLA)-DATDI(9,J)*SINPHI*SIN(THETAF+ZLA)
  XCORSTA(1)=XCORSTA(1)+DR*COSLA*COSPHI-DE*SINLA-DN*SINPHI*COSLA  
  XCORSTA(2)=XCORSTA(2)+DR*SINLA*COSPHI+DE*COSLA-DN*SINPHI*SINLA  
  XCORSTA(3)=XCORSTA(3)+DR*SINPHI+DN*COSPHI  
ENDDO   

DO I=1,3
  XCORSTA(I)=XCORSTA(I)/1000.
ENDDO  
RETURN  
END  
!  
!  *************************************************************  
!
SUBROUTINE STEP2LON(XSTA,FHR,T,XCORSTA)  
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DOUBLE PRECISION deg2rad
DOUBLE PRECISION XSTA(3),XCORSTA(3),DATDI(9,5)
DATA deg2rad/0.0174532925d0/ 
DATA ((DATDI(i,j),i=1,9),j=1,5)/ & 
   0, 0, 0, 1, 0,   0.47, 0.23, 0.16, 0.07,&
   0, 2, 0, 0, 0,  -0.20,-0.12,-0.11,-0.05,&
   1, 0,-1, 0, 0,  -0.11,-0.08,-0.09,-0.04,&
   2, 0, 0, 0, 0,  -0.13,-0.11,-0.15,-0.07,&
   2, 0, 0, 1, 0,  -0.05,-0.05,-0.06,-0.03/
!
S=218.31664563D0+481267.88194D0*T-0.0014663889D0*T**2+0.00000185139D0*T**3 
PR=1.396971278*T+0.000308889*T**2+0.000000021*T**3+0.000000007*T**4 
S=S+PR 
H=280.46645D0+36000.7697489D0*T+0.00030322222D0*T**2+0.000000020*T**3-0.00000000654*T**4 
P=83.35324312D0+4069.01363525D0*T-0.01032172222D0*T**2-0.0000124991D0*T**3+0.00000005263D0*T**4 
ZNS=234.95544499D0 +1934.13626197D0*T-0.00207561111D0*T**2-0.00000213944D0*T**3+0.00000001650D0*T**4
PS=282.93734098D0+1.71945766667D0*T+0.00045688889D0*T**2-0.00000001778D0*T**3-0.00000000334D0*T**4 
RSTA=DSQRT(XSTA(1)**2+XSTA(2)**2+XSTA(3)**2)  
SINPHI=XSTA(3)/RSTA  
COSPHI=DSQRT(XSTA(1)**2+XSTA(2)**2)/RSTA  
COSLA=XSTA(1)/COSPHI/RSTA
SINLA=XSTA(2)/COSPHI/RSTA
! reduce angles to between 0 and 360.
s=  dmod(s,360.d0)
tau=dmod(tau,360.d0)
h=  dmod(h,360.d0)
p=  dmod(p,360.d0)
zns=dmod(zns,360.d0)
ps=dmod(ps,360.d0)

dr_tot=0.
dn_tot=0.
DO I=1,3  
  XCORSTA(I)=0.
ENDDO
DO J=1,5
  THETAF=(DATDI(1,J)*S+DATDI(2,J)*H+DATDI(3,J)*P+DATDI(4,J)*ZNS+DATDI(5,J)*PS)*DEG2RAD
  DR=DATDI(6,J)*(3.*SINPHI**2-1.)/2.*COS(THETAF)+DATDI(8,J)*(3.*SINPHI**2-1.)/2.*SIN(THETAF)
  DN=DATDI(7,J)*(COSPHI*SINPHI*2.)*COS(THETAF)+DATDI(9,J)*(COSPHI*SINPHI*2.)*SIN(THETAF)
  DE=0. 
  dr_tot=dr_tot+dr
  dn_tot=dn_tot+dn
  XCORSTA(1)=XCORSTA(1)+DR*COSLA*COSPHI-DE*SINLA-DN*SINPHI*COSLA  
  XCORSTA(2)=XCORSTA(2)+DR*SINLA*COSPHI+DE*COSLA-DN*SINPHI*SINLA  
  XCORSTA(3)=XCORSTA(3)+DR*SINPHI+DN*COSPHI  
ENDDO   

DO I=1,3
  XCORSTA(I)=XCORSTA(I)/1000.
ENDDO  
RETURN  
END  
!**************************************************************************************************
! 
SUBROUTINE ST1IDIU(XSTA,XSUN,XMON,FAC2SUN,FAC2MON,XCORSTA)
!  
! THIS SUBROUTINE GIVES THE OUT-OF-PHASE CORRECTIONS INDUCED BY 
! MANTLE INELASTICITY IN THE DIURNAL BAND 
!  
!       INPUT : XSTA,XSUN,XMON,FAC2SUN,FAC2MON  
!      OUTPUT : XCORSTA  
!  
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DOUBLE PRECISION DOT
DIMENSION XSTA(3),XSUN(3),XMON(3),XCORSTA(3)  
DATA DHI/-0.0025/,DLI/-0.0007/  
RSTA=DSQRT(DOT(3,XSTA,XSTA))
SINPHI=XSTA(3)/RSTA  
COSPHI=DSQRT(XSTA(1)**2+XSTA(2)**2)/RSTA
COS2PHI=COSPHI**2-SINPHI**2
SINLA=XSTA(2)/COSPHI/RSTA  
COSLA=XSTA(1)/COSPHI/RSTA  
RMON=DSQRT(DOT(3,XMON,XMON))
RSUN=DSQRT(DOT(3,XSUN,XSUN))
DRSUN=-3.*DHI*SINPHI*COSPHI*FAC2SUN*XSUN(3)*(XSUN(1)*SINLA-XSUN(2)*COSLA)/RSUN**2
DRMON=-3.*DHI*SINPHI*COSPHI*FAC2MON*XMON(3)*(XMON(1)*SINLA-XMON(2)*COSLA)/RMON**2
DNSUN=-3.*DLI*COS2PHI*FAC2SUN*XSUN(3)*(XSUN(1)*SINLA-XSUN(2)*COSLA)/RSUN**2
DNMON=-3.*DLI*COS2PHI*FAC2MON*XMON(3)*(XMON(1)*SINLA-XMON(2)*COSLA)/RMON**2
DESUN=-3.*DLI*SINPHI*FAC2SUN*XSUN(3)*(XSUN(1)*COSLA+XSUN(2)*SINLA)/RSUN**2
DEMON=-3.*DLI*SINPHI*FAC2MON*XMON(3)*(XMON(1)*COSLA+XMON(2)*SINLA)/RMON**2
DR=DRSUN+DRMON 
DN=DNSUN+DNMON  
DE=DESUN+DEMON 
XCORSTA(1)=DR*COSLA*COSPHI-DE*SINLA-DN*SINPHI*COSLA  
XCORSTA(2)=DR*SINLA*COSPHI+DE*COSLA-DN*SINPHI*SINLA  
XCORSTA(3)=DR*SINPHI+DN*COSPHI  
RETURN  
END  
!
!-------------------------------------------------------------------------
SUBROUTINE ST1ISEM(XSTA,XSUN,XMON,FAC2SUN,FAC2MON,XCORSTA)
!  
! THIS SUBROUTINE GIVES THE OUT-OF-PHASE CORRECTIONS INDUCED BY 
! MANTLE INELASTICITY IN THE DIURNAL BAND 
!  
!       INPUT : XSTA,XSUN,XMON,FAC2SUN,FAC2MON  
!      OUTPUT : XCORSTA  
!  
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
DOUBLE PRECISION DOT
DIMENSION XSTA(3),XSUN(3),XMON(3),XCORSTA(3)  
DATA DHI/-0.0022/,DLI/-0.0007/  
RSTA=DSQRT(DOT(3,XSTA,XSTA))
SINPHI=XSTA(3)/RSTA  
COSPHI=DSQRT(XSTA(1)**2+XSTA(2)**2)/RSTA
SINLA=XSTA(2)/COSPHI/RSTA  
COSLA=XSTA(1)/COSPHI/RSTA  
COSTWOLA=COSLA**2-SINLA**2  
SINTWOLA=2.*COSLA*SINLA  
RMON=DSQRT(DOT(3,XMON,XMON))
RSUN=DSQRT(DOT(3,XSUN,XSUN))
DRSUN=-3./4.*DHI*COSPHI**2*FAC2SUN*((XSUN(1)**2-XSUN(2)**2)*SINTWOLA-2.*XSUN(1)*XSUN(2)*COSTWOLA)/RSUN**2  
DRMON=-3./4.*DHI*COSPHI**2*FAC2MON*((XMON(1)**2-XMON(2)**2)*SINTWOLA-2.*XMON(1)*XMON(2)*COSTWOLA)/RMON**2  
DNSUN=3./2.*DLI*SINPHI*COSPHI*FAC2SUN*((XSUN(1)**2-XSUN(2)**2)*SINTWOLA-2.*XSUN(1)*XSUN(2)*COSTWOLA)/RSUN**2  
DNMON=3./2.*DLI*SINPHI*COSPHI*FAC2MON*((XMON(1)**2-XMON(2)**2)*SINTWOLA-2.*XMON(1)*XMON(2)*COSTWOLA)/RMON**2  
DESUN=-3./2.*DLI*COSPHI*FAC2SUN*((XSUN(1)**2-XSUN(2)**2)*COSTWOLA+2.*XSUN(1)*XSUN(2)*SINTWOLA)/RSUN**2  
DEMON=-3./2.*DLI*COSPHI*FAC2MON*((XMON(1)**2-XMON(2)**2)*COSTWOLA+2.*XMON(1)*XMON(2)*SINTWOLA)/RMON**2  
DR=DRSUN+DRMON 
DN=DNSUN+DNMON  
DE=DESUN+DEMON 
XCORSTA(1)=DR*COSLA*COSPHI-DE*SINLA-DN*SINPHI*COSLA  
XCORSTA(2)=DR*SINLA*COSPHI+DE*COSLA-DN*SINPHI*SINLA  
XCORSTA(3)=DR*SINPHI+DN*COSPHI  
RETURN  
END  

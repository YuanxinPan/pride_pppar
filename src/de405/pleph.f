C
      SUBROUTINE PLEPH ( ET, NTARG, NCENT, RRD )
C
C 
C                   JPL PLANETARY AND LUNAR EPHEMERIDES
C 
C                            E M Standish, JPL
C 
C 
C Copyright 1997, The Jet Propulsion Laboratory, California Institute of
C Technology, All Rights Reserved.
C 
C Published by Willmann-Bell, Inc., P.O. Box 35025, Richmond, VA 23235.
C 
C The programs and data provided in this CD-ROM are based upon on a long-term and
C ongoing scientific program of analysis and refinement. What is presented here
C has been tested with care but is not guaranteed for any particular purpose.
C Neither The Jet Propulsion Laboratory nor Willmann-Bell, Inc. offer any
C warranties or representations, nor do they accept any liabilities with respect
C to the contents of this CD-ROM.
C
C  For de405, set KSIZE to 2036  NOTE : Over the years, different versions of
C  PLEPH have had a fifth argument:
C  sometimes, an error return statement number; sometimes, a logical denoting
C  whether or not the requested
C  For  de405, set KSIZE to 2036
C  For  de405, set KSIZE to 2036 date is covered by the ephemeris.  We apologize
C  for this inconsistency; in this present version, we use only the four necessary
C  arguments and do the testing outside of the subroutine.
C
C     THIS SUBROUTINE READS THE JPL PLANETARY EPHEMERIS
C     AND GIVES THE POSITION AND VELOCITY OF THE POINT 'NTARG'
C     WITH RESPECT TO 'NCENT'.
C
C     CALLING SEQUENCE PARAMETERS:
C
C       ET = D.P. JULIAN EPHEMERIS DATE AT WHICH INTERPOLATION
C            IS WANTED.
C
C       ** NOTE THE ENTRY DPLEPH FOR A DOUBLY-DIMENSIONED TIME **
C          THE REASON FOR THIS OPTION IS DISCUSSED IN THE
C          SUBROUTINE STATE
C
C     NTARG = INTEGER NUMBER OF 'TARGET' POINT.
C
C     NCENT = INTEGER NUMBER OF CENTER POINT.
C
C            THE NUMBERING CONVENTION FOR 'NTARG' AND 'NCENT' IS:
C
C                1 = MERCURY           8 = NEPTUNE
C                2 = VENUS             9 = PLUTO
C                3 = EARTH            10 = MOON
C                4 = MARS             11 = SUN
C                5 = JUPITER          12 = SOLAR-SYSTEM BARYCENTER
C                6 = SATURN           13 = EARTH-MOON BARYCENTER
C                7 = URANUS           14 = NUTATIONS (LONGITUDE AND OBLIQ)
C                            15 = LIBRATIONS, IF ON EPH FILE
C
C             (IF NUTATIONS ARE WANTED, SET NTARG = 14. FOR LIBRATIONS,
C              SET NTARG = 15. SET NCENT=0.)
C
C      RRD = OUTPUT 6-WORD D.P. ARRAY CONTAINING POSITION AND VELOCITY
C            OF POINT 'NTARG' RELATIVE TO 'NCENT'. THE UNITS ARE AU AND
C            AU/DAY. FOR LIBRATIONS THE UNITS ARE RADIANS AND RADIANS
C            PER DAY. IN THE CASE OF NUTATIONS THE FIRST FOUR WORDS OF
C            RRD WILL BE SET TO NUTATIONS AND RATES, HAVING UNITS OF
C            RADIANS AND RADIANS/DAY.
C
C            The option is available to have the units in km and km/sec.
C            For this, set km=.true. in the STCOMX common block.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      DIMENSION RRD(6),ET2Z(2),ET2(2),PV(6,13)
      DIMENSION PVSUN(6)

      LOGICAL BSAVE,BARY,FIRST1

      INTEGER LIST(12)
      data EMRAT,FIRST1/0.D0,.TRUE./
      SAVE FIRST1
      COMMON/STCOMX/BARY,PVSUN

C     INITIALIZE ET2 FOR 'STATE' AND SET UP COMPONENT COUNT
      IF(FIRST1) THEN
         FIRST1 = .FALSE.
         pv = 0.d0
      endif
      ET2(1)=ET
      ET2(2)=0.D0
      GO TO 11

C     ENTRY POINT 'DPLEPH' FOR DOUBLY-DIMENSIONED TIME ARGUMENT
C          (SEE THE DISCUSSION IN THE SUBROUTINE STATE)

      ENTRY DPLEPH(ET2Z,NTARG,NCENT,RRD)

      ET2(1)=ET2Z(1)
      ET2(2)=ET2Z(2)

  11  DO I=1,6
        RRD(I)=0.D0
      ENDDO

  96  IF(NTARG .EQ. NCENT) RETURN

      DO I=1,12
        LIST(I)=0
      ENDDO

C     CHECK FOR NUTATION CALL

      IF(NTARG.NE.14) GO TO 97
        LIST(11)=2
        CALL STATE(ET2,LIST,PV,RRD)
        RETURN

C     CHECK FOR LIBRATIONS

  97  IF(NTARG.NE.15) GO TO 98
          LIST(12)=2
          CALL STATE(ET2,LIST,PV,RRD)
          DO I=1,6
          RRD(I)=PV(I,11)
          ENDDO
          RETURN

C       FORCE BARYCENTRIC OUTPUT BY 'STATE'

  98  BSAVE=BARY
      BARY=.TRUE.

C       SET UP PROPER ENTRIES IN 'LIST' ARRAY FOR STATE CALL

      DO I=1,2
      K=NTARG
      IF(I .EQ. 2) K=NCENT
      IF(K .LE. 10) LIST(K)=2
      IF(K .EQ. 10) LIST(3)=2
      IF(K .EQ. 3) LIST(10)=2
      IF(K .EQ. 13) LIST(3)=2
      ENDDO

C       MAKE CALL TO STATE

      CALL STATE(ET2,LIST,PV,RRD)

      IF(NTARG .EQ. 11 .OR. NCENT .EQ. 11) THEN
      DO I=1,6
      PV(I,11)=PVSUN(I)
      ENDDO
      ENDIF

      IF(NTARG .EQ. 12 .OR. NCENT .EQ. 12) THEN
      DO I=1,6
      PV(I,12)=0.D0
      ENDDO
      ENDIF

      IF(NTARG .EQ. 13 .OR. NCENT .EQ. 13) THEN
      DO I=1,6
      PV(I,13)=PV(I,3)
      ENDDO
      ENDIF

      IF(NTARG*NCENT .EQ. 30 .AND. NTARG+NCENT .EQ. 13) THEN
      DO I=1,6
      PV(I,3)=0.D0
      ENDDO
      GO TO 99
      ENDIF

      IF(LIST(3) .EQ. 2) THEN
      if(EMRAT.eq.0.d0) call jpleph_const('EMRAT ',EMRAT)
      DO I=1,6
      PV(I,3)=PV(I,3)-PV(I,10)/(1.D0+EMRAT)
      ENDDO
      ENDIF

      IF(LIST(10) .EQ. 2) THEN
      DO I=1,6
      PV(I,10)=PV(I,3)+PV(I,10)
      ENDDO
      ENDIF

  99  DO I=1,6
      RRD(I)=PV(I,NTARG)-PV(I,NCENT)
      IF(I.GT.3) RRD(I)=RRD(I)*86400.d0
      ENDDO

      BARY=BSAVE
      RETURN
      END

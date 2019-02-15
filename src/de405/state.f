      SUBROUTINE STATE(ET2,LIST,PV,PNUT)
C
C++++++++++++++++++++++++++++++++
C
C THIS SUBROUTINE READS AND INTERPOLATES THE JPL PLANETARY EPHEMERIS FILE
C
C     CALLING SEQUENCE PARAMETERS:
C
C     INPUT:
C
C         ET2   DP 2-WORD JULIAN EPHEMERIS EPOCH AT WHICH INTERPOLATION
C               IS WANTED.  ANY COMBINATION OF ET2(1)+ET2(2) WHICH FALLS
C               WITHIN THE TIME SPAN ON THE FILE IS A PERMISSIBLE EPOCH.
C
C                A. FOR EASE IN PROGRAMMING, THE USER MAY PUT THE
C                   ENTIRE EPOCH IN ET2(1) AND SET ET2(2)=0.
C
C                B. FOR MAXIMUM INTERPOLATION ACCURACY, SET ET2(1) =
C                   THE MOST RECENT MIDNIGHT AT OR BEFORE INTERPOLATION
C                   EPOCH AND SET ET2(2) = FRACTIONAL PART OF A DAY
C                   ELAPSED BETWEEN ET2(1) AND EPOCH.
C
C                C. AS AN ALTERNATIVE, IT MAY PROVE CONVENIENT TO SET
C                   ET2(1) = SOME FIXED EPOCH, SUCH AS START OF INTEGRATION,
C                   AND ET2(2) = ELAPSED INTERVAL BETWEEN THEN AND EPOCH.
C
C        LIST   12-WORD INTEGER ARRAY SPECIFYING WHAT INTERPOLATION
C               IS WANTED FOR EACH OF THE BODIES ON THE FILE.
C
C                         LIST(I)=0, NO INTERPOLATION FOR BODY I
C                                =1, POSITION ONLY
C                                =2, POSITION AND VELOCITY
C
C               THE DESIGNATION OF THE ASTRONOMICAL BODIES BY I IS:
C
C                         I = 1: MERCURY
C                           = 2: VENUS
C                           = 3: EARTH-MOON BARYCENTER
C                           = 4: MARS
C                           = 5: JUPITER
C                           = 6: SATURN
C                           = 7: URANUS
C                           = 8: NEPTUNE
C                           = 9: PLUTO
C                           =10: GEOCENTRIC MOON
C                           =11: NUTATIONS IN LONGITUDE AND OBLIQUITY
C                           =12: LUNAR LIBRATIONS (IF ON FILE)
C
C
C     OUTPUT:
C
C          PV   DP 6 X 11 ARRAY THAT WILL CONTAIN REQUESTED INTERPOLATED
C               QUANTITIES.  THE BODY SPECIFIED BY LIST(I) WILL HAVE ITS
C               STATE IN THE ARRAY STARTING AT PV(1,I).  (ON ANY GIVEN
C               CALL, ONLY THOSE WORDS IN 'PV' WHICH ARE AFFECTED BY THE
C               FIRST 10 'LIST' ENTRIES (AND BY LIST(12) IF LIBRATIONS ARE
C               ON THE FILE) ARE SET.  THE REST OF THE 'PV' ARRAY
C               IS UNTOUCHED.)  THE ORDER OF COMPONENTS STARTING IN
C               PV(1,I) IS: X,Y,Z,DX,DY,DZ.
C
C               ALL OUTPUT VECTORS ARE REFERENCED TO THE EARTH MEAN
C               EQUATOR AND EQUINOX OF J2000 IF THE DE NUMBER IS 200 OR
C               GREATER; OF B1950 IF THE DE NUMBER IS LESS THAN 200.
C
C               THE MOON STATE IS ALWAYS GEOCENTRIC; THE OTHER NINE STATES
C               ARE EITHER HELIOCENTRIC OR SOLAR-SYSTEM BARYCENTRIC,
C               DEPENDING ON THE SETTING OF COMMON FLAGS (SEE BELOW).
C
C               LUNAR LIBRATIONS, IF ON FILE, ARE PUT INTO PV(K,11) IF
C               LIST(12) IS 1 OR 2.
C
C         NUT   DP 4-WORD ARRAY THAT WILL CONTAIN NUTATIONS AND RATES,
C               DEPENDING ON THE SETTING OF LIST(11).  THE ORDER OF
C               QUANTITIES IN NUT IS:
C
C                        D PSI  (NUTATION IN LONGITUDE)
C                        D EPSILON (NUTATION IN OBLIQUITY)
C                        D PSI DOT
C                        D EPSILON DOT
C
C           *   STATEMENT # FOR ERROR RETURN, IN CASE OF EPOCH OUT OF
C               RANGE OR I/O ERRORS.
C
C
C     COMMON AREA STCOMX:
C
C          KM   LOGICAL FLAG DEFINING PHYSICAL UNITS OF THE OUTPUT
C               STATES. KM = .TRUE., KM AND KM/SEC
C                          = .FALSE., AU AND AU/DAY
C               DEFAULT VALUE = .FALSE.  (KM DETERMINES TIME UNIT
C               FOR NUTATIONS AND LIBRATIONS.  ANGLE UNIT IS ALWAYS RADIANS.)
C
C        BARY   LOGICAL FLAG DEFINING OUTPUT CENTER.
C               ONLY THE 9 PLANETS ARE AFFECTED.
C                        BARY = .TRUE. =\ CENTER IS SOLAR-SYSTEM BARYCENTER
C                             = .FALSE. =\ CENTER IS SUN
C               DEFAULT VALUE = .FALSE.
C
C       PVSUN   DP 6-WORD ARRAY CONTAINING THE BARYCENTRIC POSITION AND
C               VELOCITY OF THE SUN.
C
C
      IMPLICIT NONE
      INCLUDE 'jpleph.h'
      INTEGER LIST(12)
      DOUBLE PRECISION ET2(2),PV(6,12),PNUT(4),PVSUN(6,2)
      TYPE(JPLEPH_HEADER) HD
c
cc local variables
      LOGICAL KM,BARY,FIRST
      INTEGER*4 NRECL,KSIZE,NRL,NR,I,J,K,LFN,IRECSZ
      DOUBLE PRECISION T(2),PJD(4),BUF(1500),AUFAC,S

      CHARACTER*(*) CNAME
      DOUBLE PRECISION RVALE
c
cc functions called
      INTEGER*4 GET_VALID_UNIT

      DATA FIRST,KM,BARY/.TRUE.,.TRUE.,.FALSE./

      SAVE FIRST,NRL,LFN,NRECL,KSIZE,IRECSZ,HD,KM,BARY
cc lfn,nr,nrecl,ksize,buf,HD,irecsz
C
C  ENTRY POINT - 1ST TIME IN, GET POINTER DATA, ETC., FROM EPH FILE
      IF(FIRST) THEN
       FIRST=.FALSE.

C ************************************************************************
C THE USER MUST SELECT ONE OF THE FOLLOWING BY DELETING THE 'C' IN COLUMN 1
C ************************************************************************

        NRECL=4
        KSIZE=2036
        IRECSZ=NRECL*KSIZE
        lfn=get_valid_unit(10)
        OPEN(lfn,FILE='jpleph_de405',ACCESS='DIRECT',FORM='UNFORMATTED',
     +   RECL=IRECSZ,STATUS='OLD',err=200)
        READ(lfn,REC=1) HD%CNAM,HD%SS,HD%NCON,HD%AU,HD%EMRAT,
     +   ((HD%IPT(I,J),I=1,3),J=1,12),HD%NUMDE,(HD%IPT(I,13),I=1,3)
        READ(LFN,REC=2) HD%CVAL
        NRL=0
        if(ET2(1)+ET2(2).eq.0.d0) return
      ENDIF
c
cc MAIN ENTRY POINT
      S=ET2(1)-.5D0
      CALL SPLIT(S,PJD(1))
      CALL SPLIT(ET2(2),PJD(3))
      PJD(1)=PJD(1)+PJD(3)+.5D0
      PJD(2)=PJD(2)+PJD(4)
      CALL SPLIT(PJD(2),PJD(3))
      PJD(1)=PJD(1)+PJD(3)
c
cc ERROR RETURN FOR EPOCH OUT OF RANGE
      IF(PJD(1)+PJD(4).LT.HD%SS(1) .OR. PJD(1)+PJD(4).GT.HD%SS(2)) then
         WRITE(*,'(a,f12.2,a,2f12.2)')
     +   '***ERROR(jpleph_state): requested JED',ET2(1)+ET2(2),'
     +    not within ephemeris limits,',HD%SS(1),HD%SS(2)
         call exit(1)
      endif
c
cc CALCULATE RECORD # AND RELATIVE TIME IN INTERVAL
      NR=IDINT((PJD(1)-HD%SS(1))/HD%SS(3))+3
      IF(PJD(1).EQ.HD%SS(2)) NR=NR-1
      T(1)=((PJD(1)-(DBLE(NR-3)*HD%SS(3)+HD%SS(1)))+PJD(4))/HD%SS(3)
c
Cc READ CORRECT RECORD IF NOT IN CORE
C      IF(NR.NE.NRL) THEN
        NRL=NR
        READ(LFN,REC=NR,ERR=100)(BUF(K),K=1,KSIZE/2)
C      ENDIF
c
cc unit
      IF(KM) THEN
        T(2)=HD%SS(3)*86400.D0
        AUFAC=1.D0
      ELSE
        T(2)=HD%SS(3)
        AUFAC=1.D0/HD%AU
      ENDIF
c
cc INTERPOLATE SSBARY SUN
      CALL INTERP(BUF(HD%IPT(1,11)),T,HD%IPT(2,11),3,HD%IPT(3,11),
     +            2,PVSUN)
      DO I=1,6
        PVSUN(I,1)=PVSUN(I,1)*AUFAC
      ENDDO
c
cc CHECK AND INTERPOLATE WHICHEVER BODIES ARE REQUESTED
      DO I=1,10
        IF(LIST(I).EQ.0) cycle
        CALL INTERP(BUF(HD%IPT(1,I)),T,HD%IPT(2,I),3,HD%IPT(3,I),
     +        LIST(I),PV(1,I))
        DO J=1,6
          IF(I.LE.9 .AND. .NOT.BARY) THEN
            PV(J,I)=PV(J,I)*AUFAC-PVSUN(J,1)
          ELSE
            PV(J,I)=PV(J,I)*AUFAC
          ENDIF
        ENDDO
      ENDDO
c
Cc DO NUTATIONS IF REQUESTED (AND IF ON FILE)
      IF(LIST(11).GT.0 .AND. HD%IPT(2,12).GT.0) then
        CALL INTERP(BUF(HD%IPT(1,12)),T,HD%IPT(2,12),2,HD%IPT(3,12),
     +             LIST(11),PNUT)
      endif
c
Cc GET LIBRATIONS IF REQUESTED (AND IF ON FILE)
      IF(LIST(12).GT.0 .AND. HD%IPT(2,13).GT.0) then
        CALL INTERP(BUF(HD%IPT(1,13)),T,HD%IPT(2,13),3,HD%IPT(3,13),
     +            LIST(12),PV(1,11))
      endif

      RETURN

  100 WRITE(*,'(a,2F12.2,A)') '***ERROR(jpleph_state):
     +            read `jpleph_de405` for time,',ET2
      call exit(1)

  200 WRITE(*,'(a,2F12.2,A)') '***ERROR(jpleph_state):
     +		  open `jpleph_de405` '
      call exit(1)



c
cc
      ENTRY JPLEPH_CONST(CNAME,RVALE)
      IF(FIRST) THEN
        FIRST=.FALSE.

C ************************************************************************
C THE USER MUST SELECT ONE OF THE FOLLOWING BY DELETING THE 'C' IN COLUMN 1
C ************************************************************************

        NRECL=4
        KSIZE=2036
        IRECSZ=NRECL*KSIZE
        lfn=get_valid_unit(10)
        OPEN(lfn,FILE='jpleph_de405',ACCESS='DIRECT',
     +       FORM='UNFORMATTED',RECL=IRECSZ,STATUS='OLD',err=200)
        READ(lfn,REC=1) HD%TTL,HD%CNAM,HD%SS,HD%NCON,HD%AU,HD%EMRAT,
     +       ((HD%IPT(I,J),I=1,3),J=1,12),HD%NUMDE,(HD%IPT(I,13),I=1,3)
        READ(lfn,REC=2) HD%CVAL
        NRL=0
      ENDIF
c
cc local variables
      DO I=1,HD%NCON
        J=LEN(HD%CNAM(I))
        if(CNAME(1:J).EQ.HD%CNAM(I)) THEN
          RVALE=HD%CVAL(I)
          RETURN
        ENDIF
      ENDDO
      RVALE=0.d0
      RETURN
      END

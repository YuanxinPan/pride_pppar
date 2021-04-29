C
      SUBROUTINE SPLIT(TT,FR)
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
C+++++++++++++++++++++++++
C
C     THIS SUBROUTINE BREAKS A D.P. NUMBER INTO A D.P. INTEGER
C     AND A D.P. FRACTIONAL PART.
C
C     CALLING SEQUENCE PARAMETERS:
C
C       TT = D.P. INPUT NUMBER
C
C       FR = D.P. 2-WORD OUTPUT ARRAY.
C            FR(1) CONTAINS INTEGER PART
C            FR(2) CONTAINS FRACTIONAL PART
C
C            FOR NEGATIVE INPUT NUMBERS, FR(1) CONTAINS THE NEXT
C            MORE NEGATIVE INTEGER; FR(2) CONTAINS A POSITIVE FRACTION.
C
C       CALLING SEQUENCE DECLARATIONS
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      DIMENSION FR(2)

C       MAIN ENTRY -- GET INTEGER AND FRACTIONAL PARTS

      FR(1)=DINT(TT)
      FR(2)=TT-FR(1)

      IF(TT.GE.0.D0 .OR. FR(2).EQ.0.D0) RETURN

C       MAKE ADJUSTMENTS FOR NEGATIVE INPUT NUMBER

      FR(1)=FR(1)-1.D0
      FR(2)=FR(2)+1.D0

      RETURN

      END

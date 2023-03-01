      SUBROUTINE EXTFLD(T1)
C  Calculate effective external field on particles.
C  Since the field boundary is sharp, if a particle crosses
C  the boundary, the time fraction in the field is
C  multiplied. 
C  These treatment do not make sense for radiation
C  but will not cause a big problem.
	USE BEAMCM
      IMPLICIT NONE
      REAL*8 T1
C      INCLUDE 'include/beamcm.h'
      INCLUDE 'include/extfcm.h'
      INCLUDE 'include/cnstcm.h'
      INTEGER N,I,I1
      REAL*8 T0,DT1,S0,S1,SA,SB,FRAC
C
      IF(LEXTF.LE.0) RETURN
      DO 300 N=1,NP
        IF(LOST(N).NE.0) GOTO 300
        T0=TXYS(0,N)
        DT1=T1-T0
        FRAC=1
        IF(LEXTFB(1).NE.0.OR.LEXTFB(2).NE.0) THEN
          S0=-T0*EXTFBV(0)
          S1=-T1*EXTFBV(0)
          DO 220 I=1,3
            S0=S0+TXYS(I,N)*EXTFBV(I)
            S1=S1+(TXYS(I,N)+DT1*EP(I,N)/EP(0,N))*EXTFBV(I)
 220      CONTINUE
          SA=MIN(S0,S1)
          SB=MAX(S0,S1)
          IF(SA.EQ.SB) THEN
            IF(SA.LE.EXTFSS(1).OR.SA.GE.EXTFSS(2)) GOTO 300
          ELSE
            S0=MAX(SA,EXTFSS(1))
            S1=MIN(SB,EXTFSS(2))
            IF(S0.GE.S1) GOTO 300
            FRAC=(S1-S0)/(SB-SA)*FRAC
          ENDIF
        ENDIF
        DO 260 I1=1,3
          FLD(I1,1,N)=FLD(I1,1,N)+FRAC*EXTFEB(I1,1)
          FLD(I1,2,N)=FLD(I1,2,N)+FRAC*EXTFEB(I1,2)*CVEL
 260    CONTINUE
 300  CONTINUE
      RETURN
      END

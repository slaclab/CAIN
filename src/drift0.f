      SUBROUTINE DRIFT0(LTS,TS,N1,N20,LR,KIN)
C  Drift to T=TS (if LTS=1) or T-T0=TS (LTS=2)
C  or to S=TS (LTS=3)
C  Only particles between N=N1 to N20.
C  Only those with type flag LR and KIN on.
	USE BEAMCM
      IMPLICIT NONE
      INTEGER LTS,N1,N20,LR(2),KIN(3)
      REAL*8 TS
C      INCLUDE 'include/beamcm.h'
      INTEGER N,I,II,N2,IFLRKN
      REAL*8 DT,VS
      INTEGER ITYP(3)/1,1,1/
C
      IF(LTS.LE.0.OR.LTS.GE.4) RETURN
      IF(N20.EQ.0) THEN
        N2=NP
      ELSE
        N2=N20
      ENDIF
      IF(N2-N1.LT.0) RETURN
      IF(LTS.EQ.2) THEN
        IF(TS.EQ.0) RETURN
        DT=TS
      ENDIF
      CALL CPUTIM('DRIFT0',1)
      II=3
      IF(LTS.EQ.3) II=2
      DO 240 N=N1,N2
        IF(IFLRKN(N,LR,KIN,0,0,0,ITYP).EQ.0) GOTO 240
        IF(LTS.EQ.1) THEN
          DT=TS-TXYS(0,N)
          TXYS(0,N)=TS
        ELSEIF(LTS.EQ.2) THEN
          TXYS(0,N)=TXYS(0,N)+DT
        ELSE
          VS=EP(3,N)/EP(0,N)
          IF(VS.EQ.0) THEN
            LOST(N)=1
            GOTO 240
          ENDIF
          DT=(TS-TXYS(3,N))/VS
          TXYS(3,N)=TS
          TXYS(0,N)=TXYS(0,N)+DT
        ENDIF
        DO 220 I=1,II
          TXYS(I,N)=TXYS(I,N)+EP(I,N)/EP(0,N)*DT
 220    CONTINUE
 240  CONTINUE
      CALL CPUTIM('DRIFT0',2)
      RETURN
      END

      SUBROUTINE DRIFTX(LTS,TS,N1,N20,LR,KIN)
C  Drift to T=TS (if LTS=1) or T-T0=TS (LTS=2) in external field
C  or to S=TS (LTS=3)
C  Only particles between N=N1 to N20.
C  Only those with type flag LR and KIN on.
	USE BEAMCM
      IMPLICIT NONE
      INTEGER LTS,N1,N20,LR(2),KIN(3)
      REAL*8 TS
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/beamcm.h'
      INCLUDE 'include/extfcm.h'
      INTEGER LTS1,N,I,I0,N2,IRTN,IFLRKN
      REAL*8 DTS,CHARG
      LOGICAL LLEB(2)
      INTEGER ITYPE(3)/1,1,1/
C
      IF(LTS.LE.0.OR.LTS.GE.4) RETURN
      IF(LEXTF.EQ.0) THEN
        CALL DRIFT0(LTS,TS,N1,N20,LR,KIN)
        RETURN
      ENDIF
      IF(N20.EQ.0) THEN
        N2=NP
      ELSE
        N2=N20
      ENDIF
      IF(N2-N1.LT.0) RETURN
      IF(LTS.EQ.2) THEN
        IF(TS.EQ.0) RETURN
        DTS=TS
      ENDIF
      CALL CPUTIM('DRIFTX',1)
      IF(LTS.LE.2) THEN
        LTS1=1
        I0=0
      ELSE
        LTS1=2
        I0=3
      ENDIF
      LLEB(1)=EXTFEB(1,1).NE.0.OR.EXTFEB(2,1).NE.0.OR.EXTFEB(3,1).NE.0
      LLEB(2)=EXTFEB(1,2).NE.0.OR.EXTFEB(2,2).NE.0.OR.EXTFEB(3,2).NE.0
      DO 300 N=N1,N2
        IF(IFLRKN(N,LR,KIN,0,0,0,ITYPE).EQ.0) GOTO 300
        IF(LTS.NE.2) DTS=TS-TXYS(I0,N)
        IF(KIND(N).EQ.1.OR.(.NOT.LLEB(1).AND..NOT.LLEB(2))) THEN
          IF(EP(I0,N).EQ.0) THEN
            LOST(N)=1
          ELSE
            DO 240 I=0,3
              TXYS(I,N)=TXYS(I,N)+DTS*EP(I,N)/EP(I0,N)
 240        CONTINUE
          ENDIF
        ELSE
          CHARG=1
          IF(KIND(N).EQ.2) CHARG=-1
          CALL DRFEXT(LTS1,DTS,CHARG,TXYS(0,N),EP(0,N),ISPIN,
     %         SPIN(1,N),LLEB,EXTFEB,LEXTFB,EXTFBV,EXTFSS,IRTN)
          IF(IRTN.NE.0) LOST(N)=1
        ENDIF
 300  CONTINUE
      CALL CPUTIM('DRIFTX',2)
      RETURN
      END

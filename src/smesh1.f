      SUBROUTINE SMESH1(T,DS,SMM,NS,EMAX,IRTN)
C  Determine longitudinal mesh
C  Include test particles, ignore lost particles.
	USE BEAMCM
      IMPLICIT NONE
      INTEGER NS,IRTN
      REAL*8 T,DS,SMM(2),EMAX(2)
C      INCLUDE 'include/beamcm.h'
      INTEGER L,N,NNP(2)
      REAL*8 SMML(2,2),S0,S1
C
      IF(NP.LE.0) GOTO 900
      DO 200 L=1,2
        SMML(1,L)=1D60
        SMML(2,L)=-1D60
        NNP(L)=0
        EMAX(L)=0
 200  CONTINUE
	NS=0      !!  Added on Sep.21 according to Filip Zarnecki
      DO 220 N=1,NP
        IF(LOST(N).NE.0) GOTO 220
        L=1
        IF(EP(3,N).LT.0) L=2
        S1=TXYS(3,N)+EP(3,N)/EP(0,N)*(T-TXYS(0,N))
        SMML(1,L)=MIN(SMML(1,L),S1)
        SMML(2,L)=MAX(SMML(2,L),S1)
        NNP(L)=NNP(L)+1
        IF(PNAME(N).EQ.'    '.AND.KIND(N).NE.1)
     %      EMAX(L)=MAX(EMAX(L),EP(0,N))
C        EMAX: max.energy of charged particles.
C        Needed for beam-beam energy scale.
 220  CONTINUE
      IF(EMAX(1).EQ.0) THEN
        EMAX(1)=EMAX(2)
      ELSEIF(EMAX(2).EQ.0) THEN
        EMAX(2)=EMAX(1)
      ENDIF
      IF(NNP(1).LE.0.OR.NNP(2).LE.0) GOTO 900
      SMM(1)=MAX(SMML(1,1),SMML(1,2))
      SMM(2)=MIN(SMML(2,1),SMML(2,2))
      IF(SMM(1).GT.SMM(2)) RETURN
      S0=(SMM(2)+SMM(1))/2
      NS=INT((SMM(2)-SMM(1))/DS+1)
      SMM(1)=S0-NS*DS/2
      SMM(2)=S0+NS*DS/2
      DO 240 N=1,NP
        IF(LOST(N).NE.0) GOTO 240
        S1=TXYS(3,N)+EP(3,N)/EP(0,N)*(T-TXYS(0,N))
        ISBIN(N)=MAX(0,INT((S1-SMM(1))/DS+1))
        IF(ISBIN(N).GT.NS) ISBIN(N)=0
 240  CONTINUE
      IRTN=0
      RETURN
 900  IRTN=1
      RETURN
      END

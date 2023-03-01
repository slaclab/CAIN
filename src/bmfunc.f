      RECURSIVE FUNCTION BMFUNC(LR,KIN,K1A,K1B,K2,FSEL,IRTN)
	USE FLCHTYP
	USE BEAMCM
	USE ARRAYMOD
C  Calculate average (K2=1) or rms (K2=2)
C  of T(K1A=1), X(K1A=2), Y(K1A=3), S(K1A=4),
C     En(K1A=5), Px(K1A=6), Py(K1A=7), Ps(K1A=8)
C     Sx(K1A=9), Sy(K1A=10), Ss(K1A=11),
C     Xi1(K1A=12), Xi2(K1A=13), Xi3(K1A=14)
C         (K1B not used in these cases)
C  or calculate beam matrix (K2=3)
C     1<=K1A<=8, 1<=K1B<=8
C       e.g., BMFUNC(*,*,2,6,3,f,*) will compute
C        the average of (x-<x>)*(Px-<Px>) 
C  for right-going (LR=1) or left-going (LR=2) or both (LR=3),
C  for photon(KIN=1), electron(KIN=2), positron(KIN=3).
C  If f is not blanck, select particles eval(f) != 0.
C  In the special case K1A=0, number of real (macro) particle
C  is returned if K2=1 (2).
C  
      IMPLICIT NONE
      INTEGER LR,KIN,K1A,K1B,K2,IRTN
      REAL*8 BMFUNC
	CHARACTER(*) FSEL
C      INCLUDE 'include/beamcm.h'
      INTEGER N,KA,KB,JA,JB,ISUM,IDLM,IRTN1
      REAL*8 WSUM,AVA,AVB,RMS,XA,XB
	TYPE(FLCHTYPE) FSEL1
	CHARACTER(80) ERR
C
      BMFUNC=0
      IRTN=1000
      IF(LR.LE.0.OR.LR.GT.3) RETURN
      IF(KIN.LE.0.OR.KIN.GT.3) RETURN
      IF(K2.LE.0.OR.K2.GT.3) RETURN
	IF(K2.EQ.1.OR.K2.EQ.2) THEN
	  IF(K1A.LT.0.OR.K1A.GT.14) RETURN
	ELSE
        IF(K1A.LE.0.OR.K1A.GT.8.OR.K1B.LE.0.OR.K1B.GT.8) RETURN
	ENDIF
	IDLM=0
	IF(FSEL.NE.' ') THEN
	  CALL EVCMPL(FSEL,IDLM,ERR)
	  IF(ERR.NE.' ') RETURN
	ENDIF
      IRTN=10
      IF(NP.LE.0) GOTO 800
      ISUM=0
      WSUM=0
      AVA=0
	IF(K1A.LE.8) THEN
	  JA=(K1A-1)/4+1
	  KA=K1A-4*(JA-1)-1
C         JA=1 coord., 2 for energy-mom
      ELSE
C         JA=3 for spin
	  JA=3
	  KA=K1A-8
        IF(KA.GE.4) KA=KA-3
C          Ignore the difference between S's (elec. spin) and Xi' (photon pol)
      ENDIF
      IF(K2.EQ.3) THEN
        AVB=0
        IF(K1B.LE.4) THEN
          KB=K1B-1
          JB=1
        ELSE
          KB=K1B-5
          JB=2
        ENDIF
      ENDIF
      DO N=1,NP
        IF(KIND(N).NE.KIN) CYCLE
        IF(LR.EQ.1.AND.EP(3,N).LT.0) CYCLE
        IF(LR.EQ.2.AND.EP(3,N).GE.0) CYCLE
	  IF(LOST(N).NE.0) CYCLE
	  IF(PNAME(N)(1:1).EQ.'T') CYCLE
C      Default for incoherent particles changed on Feb.1.2002
C         When FSEL is given, incoherent particles are included by default
C                    (to exclude them, say FSEL='Incp==0')
C         When FSEL is not given, incoherent particles are excluded.
	  IF(FSEL.EQ.' ') THEN
	    IF(PNAME(N).NE.' ') CYCLE
	  ELSE
	    CALL SETVAR(N,IRTN1)
          IF(IRTN1.NE.0) CYCLE
	    CALL EVLOAD(IDLM,FSEL1,IRTN1)
	    IF(FSEL1%L.NE.1.OR.IRTN1.NE.0) CYCLE
	    IF(FSEL1%X.EQ.0) CYCLE
	  ENDIF
        WSUM=WSUM+WGT(N)
        ISUM=ISUM+1
        IF(K1A.NE.0) THEN
          IF(JA.EQ.1) THEN
            AVA=AVA+TXYS(KA,N)*WGT(N)
          ELSEIF(JA.EQ.2) THEN
            AVA=AVA+EP(KA,N)*WGT(N)
	    ELSE
	      AVA=AVA+SPIN(KA,N)*WGT(N)
          ENDIF
        ENDIF
        IF(K2.EQ.3.AND.K1B.NE.K1A) THEN
          IF(JB.EQ.1) THEN
            AVB=AVB+TXYS(KB,N)*WGT(N)
          ELSEIF(JB.EQ.2) THEN
            AVB=AVB+EP(KB,N)*WGT(N)
	    ELSE
	      AVB=AVB+SPIN(KB,N)*WGT(N)
          ENDIF
        ENDIF
      ENDDO
      IF(ISUM.EQ.0.OR.WSUM.EQ.0) GOTO 800
      IRTN=0
      IF(K1A.EQ.0) THEN
        IF(K2.EQ.1) BMFUNC=WSUM
        IF(K2.EQ.2) BMFUNC=ISUM
        GOTO 800
      ENDIF
      AVA=AVA/WSUM
      IRTN=0
      IF(K2.EQ.1) THEN
        BMFUNC=AVA
        GOTO 800
      ENDIF
      IF(K2.EQ.3) THEN
        IF(K1B.EQ.K1A) THEN
          AVB=AVA
        ELSE
          AVB=AVB/WSUM
        ENDIF
      ENDIF
C        (another do-loop for avoiding round off errors)
      RMS=0
      DO N=1,NP
        IF(KIND(N).NE.KIN) CYCLE
        IF(LR.EQ.1.AND.EP(3,N).LT.0) CYCLE
        IF(LR.EQ.2.AND.EP(3,N).GE.0) CYCLE
	  IF(LOST(N).NE.0) CYCLE
        IF(PNAME(N).NE.' ') CYCLE
	  IF(FSEL.NE.' ') THEN
	    CALL SETVAR(N,IRTN1)
          IF(IRTN1.NE.0) CYCLE
	    CALL EVLOAD(IDLM,FSEL1,IRTN1)
	    IF(FSEL1%L.NE.1.OR.IRTN1.NE.0) CYCLE
	    IF(FSEL1%X.EQ.0) CYCLE
	  ENDIF
        IF(K2.LE.2) THEN
          IF(JA.EQ.1) THEN
            RMS=RMS+(TXYS(KA,N)-AVA)**2*WGT(N)
          ELSEIF(JA.EQ.2) THEN
            RMS=RMS+(EP(KA,N)-AVA)**2*WGT(N)
	    ELSE
            RMS=RMS+(SPIN(KA,N)-AVA)**2*WGT(N)
          ENDIF
        ELSE
          IF(JA.EQ.1) THEN
            XA=TXYS(KA,N)-AVA
          ELSEIF(JA.EQ.2) THEN
            XA=EP(KA,N)-AVA
	    ELSE
	      XA=SPIN(KA,N)-AVA
          ENDIF
          IF(JB.EQ.1) THEN
            XB=TXYS(KB,N)-AVB
          ELSEIF(JB.EQ.2) THEN
            XB=EP(KB,N)-AVB
	    ELSE
            XB=SPIN(KB,N)-AVB
          ENDIF
          RMS=RMS+XA*XB*WGT(N)
        ENDIF
      ENDDO
      RMS=RMS/WSUM
      IF(K2.LE.2) RMS=SQRT(RMS)
      BMFUNC=RMS
 800  IF(IDLM.NE.0) CALL EVLMFREE(IDLM)
      RETURN
      END

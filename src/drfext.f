      SUBROUTINE DRFEXT(LTS,DTS,CHARG,TXYS,EP,ISPIN,SPIN,
     %   LLEB,EB,LBXX,XV,XX,IRTN)
C  Trajectory under a constant external field
C  Input
C    LTS     1 (give change of time), 2: (change of s)
C    DTS     delta t (LTS=1) or delta s (LTS=2)
C            Can be negative
C    CHARG   Charge in units of positron charge.
C    TXYS    Initial coordinate
C    EP      Initial energy momentum (eV/c**2,eV/c)
C    ISPIN   Flag for spin calculation
C    SPIN(i) spin component (i=1,2,3)
C    LLEB(k) logical. If .false., EB(*,k)=0.
C    EB(i,k)  External field. (x,y,z) for i=(1,2,3)
C                k=1: Electric field (V/m)
C                k=2: Magnetic field (Tesla)
C    XV,XX   Defines the boundary of the field.
C            XX(1) < -XV(0)*T+XV(1)*X+XV(2)*Y+XV(3)*s < XX(2)
C    LBXX(i) If LBXX(i)=0, boundary XX(i) must be ignored.
C  Output
C    IRTN    0: normal
C            1: cannot reach the final point (this can happen
C               only when LTS=2)
C           >1: fatal
C    
      IMPLICIT NONE
      INTEGER LTS,LBXX(2),ISPIN,IRTN
      REAL*8 DTS,CHARG,TXYS(0:3),EP(0:3),SPIN(3),EB(3,2),XV(0:3),XX(2)
      INTEGER I,J,II1,II2,I0,K,ITR,ICASE
      REAL*8 TXYS0(0:3),V(3),DT1,DT2,DS1,TS0,TS1,DTS1,
     %  XX0,XX1,XX2,DXX1,
     %  MAT(0:3,2,2),XVM(2,2),XVM1,TAU,TAU1,DTAU,DTAU1,DTAU2,EPSTAU,
     %  A(2,2),DA(2,2)
      LOGICAL LLEB(2)
      LOGICAL NOFLD,SAME
      REAL*8 CVEL/2.99792458D8/,EMASS/0.51099906D6/
      REAL*8 EB0(3,2)/0,0,0,0,0,0/,CHARG0/1/
      REAL*8 EB1(3,2),EXB(3),OMSQ(2),OM(2),
     %    CV1(3,2),CV2(3,2),CB(2),CE(2),C0
	LOGICAL LIMX,LIMT
      SAVE EB0,EB1,EXB,OMSQ,OM,CV1,CV2,CB,CE,C0,CHARG0
C
      IF(LTS.LE.0.OR.LTS.GE.3) GOTO 900
      IRTN=0
      IF(DTS.EQ.0) RETURN
      DO 200 I=1,3
        V(I)=EP(I)/EP(0)
 200  CONTINUE
      TS0=DTS
      IF(LTS.EQ.1) THEN
        DT1=DTS
        I0=0
      ELSE
        IF(V(3).EQ.0) GOTO 910
        I0=3
        DS1=DTS
        DT1=DTS/V(3)
      ENDIF
      NOFLD=.TRUE.
C--- See if the initial point is in the field
      DO 220 I=0,3
        TXYS0(I)=TXYS(I)
 220  CONTINUE
      XX1=-XV(0)*TXYS(0)+XV(1)*TXYS(1)+XV(2)*TXYS(2)+XV(3)*TXYS(3)
      IF(XX1.LT.XX(1)) THEN
        II1=1
      ELSEIF(XX1.GT.XX(2)) THEN
        II1=2
      ELSE
        II1=0
      ENDIF
      IF(II1.EQ.0) GOTO 340
C--- Straight trajectory
      DO 320 I=1,3
        TXYS(I)=TXYS(I)+DT1*V(I)
 320  CONTINUE
      TXYS(0)=TXYS(0)+DT1
      XX2=-XV(0)*TXYS(0)+XV(1)*TXYS(1)+XV(2)*TXYS(2)+XV(3)*TXYS(3)
      IF(XX2.LT.XX(1)) THEN
        II2=1
      ELSEIF(XX2.GT.XX(2)) THEN
        II2=2
      ELSE
        II2=0
      ENDIF
      IF(II2.EQ.II1) RETURN
      DT2=DT1*(XX(II1)-XX1)/(XX2-XX1)
      DO 330 I=1,3
        TXYS0(I)=TXYS0(I)+DT2*V(I)
 330  CONTINUE
      TXYS0(0)=TXYS0(0)+DT2
      IF(LTS.EQ.1) THEN
        TS0=DT1-DT2
      ELSE
        TS0=DS1*(1-(XX(II1)-XX1)/(XX2-XX1))
      ENDIF
      XX1=XX2
C--- Go into the field
C--- First compute field parameters
 340  DO 350 I=1,3
        EB1(I,1)=CHARG*EB(I,1)/EMASS
        EB1(I,2)=CHARG*EB(I,2)/EMASS*CVEL
 350  CONTINUE
      SAME=EB(1,1).EQ.EB0(1,1).AND.EB(2,1).EQ.EB0(2,1).AND.
     %     EB(3,1).EQ.EB0(3,1).AND.EB(1,2).EQ.EB0(1,2).AND.
     %     EB(2,2).EQ.EB0(2,2).AND.EB(3,2).EQ.EB0(3,2)
      IF(.NOT.SAME) THEN
        DO 360 I=1,3
          EB0(I,1)=EB(I,1)
          EB0(I,2)=EB(I,2)
 360    CONTINUE
        CHARG0=CHARG
        CALL EXTFEV(LLEB(1),LLEB(2),EB1,EXB,C0,OMSQ,OM,CV1,CV2,CE,CB)
      ELSEIF(CHARG.NE.CHARG0) THEN
C       Caution: charg=+-1 assumed.  charg=2 etc not allowed.
C       Following 1 line added on Jan.31.2002
	  CHARG0=CHARG
        DO 380 I=1,3
          DO 370 J=1,2
            CV1(I,J)=-CV1(I,J)
            CV2(I,J)=-CV2(I,J)
 370      CONTINUE
 380    CONTINUE
      ENDIF
      CALL EXFTMT(EB1,C0,CB,CE,EXB,CV1,CV2,EP,MAT)
C      For the given initial EP(0:3), the solution in the field
C      at the proper time tau is given by
C        p(i)=m*(MAT(i,1,1)*cosh(u)+MAT(i,2,1)*sinh(u)/OM(1)
C               +MAT(i,1,2)*cos(v) +MAT(i,2,2)*sin(v)/OM(2))
C        x(i)=MAT(i,1,1)*sinh(u)/OM(1)+MAT(i,2,1)*(cosh(u)-1)/OM(1)**2
C            +MAT(i,1,2)*sin(v)/OM(2)+MAT(i,2,2)*(1-cos(v))/OM(2)**2
C      where i=0,1,2,3, u=OM(1)*tau, v=OM(2)*tau,
C      m=electron mass in eV.
C
      DO 420 K=1,2
        DO 410 J=1,2
          XVM(K,J)=-XV(0)*MAT(0,K,J)+XV(1)*MAT(1,K,J)
     %             +XV(2)*MAT(2,K,J)+XV(3)*MAT(3,K,J)
 410    CONTINUE
 420  CONTINUE
C--- Guess of final point
      EPSTAU=EMASS/EP(0)*ABS(DTS)*1D-10
      TAU=TS0/(MAT(I0,1,1)+MAT(I0,1,2))
      XVM1=XVM(1,1)+XVM(1,2)
      IF(II1.EQ.0) THEN
        IF(XVM1.GE.0) THEN
          II1=2
        ELSE
          II1=1
        ENDIF
      ENDIF
      XX0=XX(II1)-XX1
      TAU1=XX0/XVM1
      IF(ABS(TAU1).LT.ABS(TAU)) TAU=TAU1
      ITR=0
 450  ITR=ITR+1
      IF(ITR.GT.20) GOTO 920
C          Loop algorithm changed (2005.11.9. Bug pointed out by Zarnecki)
      CALL EXTFAA(OM,TAU,A,DA)
      XX1=XVM(1,1)*A(1,1)+XVM(2,1)*A(2,1)
     %   +XVM(1,2)*A(1,2)+XVM(2,2)*A(2,2)
      TS1=MAT(I0,1,1)*A(1,1)+MAT(I0,2,1)*A(2,1)
     %   +MAT(I0,1,2)*A(1,2)+MAT(I0,2,2)*A(2,2)
	LIMX=XX1/XX0.GT.1
	LIMT=TS1/TS0.GT.1
	IF(LIMX.AND..NOT.LIMT) THEN
	  ICASE=1
	ELSEIF(LIMT.AND..NOT.LIMX) THEN
	  ICASE=2
	ELSE
	  ICASE=0
	ENDIF
	IF(ICASE.EQ.0.OR.ICASE.EQ.1) THEN
C   Find TAU to reach the field border
        DXX1=XVM(1,1)*DA(1,1)+XVM(2,1)*DA(2,1)
     %      +XVM(1,2)*DA(1,2)+XVM(2,2)*DA(2,2)
        DTAU1=-(XX1-XX0)/DXX1
	ENDIF
	IF(ICASE.EQ.0.OR.ICASE.EQ.2) THEN
C   Find TAU to reach the specified t(s) interval
        DTS1=MAT(I0,1,1)*DA(1,1)+MAT(I0,2,1)*DA(2,1)
     %      +MAT(I0,1,2)*DA(1,2)+MAT(I0,2,2)*DA(2,2)
        DTAU2=-(TS1-TS0)/DTS1
	ENDIF
	IF(ICASE.EQ.0) THEN
	  IF(ABS(DTAU1).LE.ABS(DTAU2)) THEN
	    ICASE=1
	  ELSE
	    ICASE=2
	  ENDIF
	ENDIF
	IF(ICASE.EQ.1) THEN
	  DTAU=DTAU1
	ELSE
	  DTAU=DTAU2
	ENDIF
	TAU=TAU+DTAU
      IF(ABS(DTAU).GE.EPSTAU) GOTO 450
      CALL EXTFAA(OM,TAU,A,DA)
      DO 480 I=0,3
        EP(I)=EMASS*(MAT(I,1,1)*DA(1,1)+MAT(I,2,1)*DA(2,1)
     %              +MAT(I,1,2)*DA(1,2)+MAT(I,2,2)*DA(2,2))
        TXYS(I)=MAT(I,1,1)*A(1,1)+MAT(I,2,1)*A(2,1)
     %         +MAT(I,1,2)*A(1,2)+MAT(I,2,2)*A(2,2)
 480  CONTINUE
      IF(ISPIN.GE.1) CALL EXFSPN(SPIN,LLEB(1),LLEB(2),EB1,
     %     MAT,OM,TAU)
C--- Get out of the field
      DTS1=DTS-TXYS(I0)
      IF(DTS1.NE.0) THEN
        IF(LTS.EQ.1) THEN
          DT1=DTS1
        ELSE
C          DT1=EP(3)/EP(0)*DTS1   replaced owing to Zarnecki (2005.11.9)
          DT1=EP(0)/EP(3)*DTS1
        ENDIF
        DO 520 I=0,3
          TXYS(I)=TXYS(I)+DT1*EP(I)/EP(0)
 520    CONTINUE
      ENDIF
      DO 540 I=0,3
        TXYS(I)=TXYS0(I)+TXYS(I)
 540  CONTINUE
      RETURN
 900  IRTN=1000
      RETURN
 910  IRTN=1
      RETURN
 920  IRTN=2
      RETURN
      END

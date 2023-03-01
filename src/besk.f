C******************** BESK *********************************
      FUNCTION BESK(ANU0,X0,LL)
C  MODIFIED BESSEL FUNCTION OF REAL ORDER: K(ANU,X) X>0.
C  All REAL*8 but accuracy is REAL*4.
C
C   BESK(A,X,1) = K(A,X)
C   BESL(A,X,2) = K(A,X)*EXP(X)
C
C  IF N**2*(N**2+X**2)>4940   , DEBYE'S ASYMPTOTIC EXPANSION
C  IF INTEGER ORDER           , MSL ABESK
C  IF HALF INTEGER ORDER      , RECCURENCE RELATION OF SPHERICAL BES.
C  IF X>5                     , CONTINUED FRACTION
C  IF CLOSE TO AN INTEGER     , K(N,X)+DN*(DK/DN)
C    OTHERWISE                , TAYLOR EXPANSION.
      IMPLICIT NONE
      INTEGER LL
      REAL*8 ANU0,X0,BESK
      REAL*8 UN(27)/
     1  +212.57013   D0, -765.25247   D0, 1059.9905    D0,
     2  -699.57963   D0, +218.19051   D0,  -26.491430  D0,
     3    +0.57250142D0,
     4   +28.212073  D0,  -84.636218  D0,  +91.818242  D0,
     5   -42.534999  D0,   +7.3687944 D0,   -0.22710800D0,
     6    +4.6695844 D0,  -11.207003  D0,   +8.7891235 D0,
     7    -2.3640869 D0,   +0.11215210D0,
     8    +1.0258126 D0,   -1.8464627 D0,   +0.89121094D0,
     9    -7.32421875D-2,
     A    +0.33420139D0,   -0.40104167D0,   +0.0703125 D0,
     B    +0.20833333D0,   -0.125     D0/
      REAL*8 PAI/3.141592653589793238D0/,HPAI/1.570796326794896619D0/,
     %     P1/4.94D3/,P2/1.13D4/,P3/5.04D4/,
     %     P4/4.91D5/,P5/3.43D7/,P6/4.37D11/
      INTEGER LIBRTN,LIBMSGLVL
      CHARACTER*80 LIBMSG
      COMMON/LIBCOM/LIBRTN,LIBMSGLVL
      COMMON/LIBCOM2/LIBMSG
      INTEGER NU,NU1,KIN,N,I,J
      REAL*8 X,ANU,ANU2,ANUX,P,FRNU,FRNU1,XH,XH2,C1,C2,C3,FF,AN,AN1,
     %  ANN,A(20),F1,F2,F3,F4,F5,F6,F7,F8,XHI,S,SQ,AK,T,TT,XIH,
     %  FF1,FF2,FF3,ER,AA,G0,G1,H0,H1,SAV,SAV1,SAV2,FSAV,ESAV
      REAL*8 DGAMMA,ASINH
      REAL*8 ABESK
C
      BESK=0
      IF(LL.LE.0.OR.LL.GE.3) GOTO 900
      IF(X0.LE.0.0) GO TO 910
C      X=DBLE(X0)
      X=X0
C      ANU=ABS(DBLE(ANU0))
      ANU=ABS(ANU0)
      ANU2=ANU*ANU
      ANUX=ANU2+X*X
      P=ANU2*ANUX
      IF(P.GE.P1) GO TO 300
C      IF(X.GE.173.0) GO TO 940
      NU=INT(ANU+0.5D0)
      FRNU=ANU-DFLOAT(NU)
      IF(ABS(FRNU).LE.2.0D-7) GO TO 400
      NU1=INT(2.0*ANU+0.5D0)
      FRNU1=ANU-DFLOAT(NU1)*0.5
      IF(ABS(FRNU1).LE.2.0D-7) GO TO 420
      IF(X.GE.5.0D0) GO TO 500
      IF(ABS(FRNU).LE.0.5D-3) GO TO 200
C------ TAYLOR EXPANSION ------
      XH=0.5D0*X
      XH2=XH*XH
      C1=XH**ANU/DGAMMA(ANU+1.0D0)
      C2=0.5D0/(C1*ANU)
      C1=C1*HPAI/SIN(PAI*ANU)
      FF=C2-C1
      AN=0.0D0
  100 AN=AN+1.0D0
      C3=XH2/AN
      C1=C1*C3/(AN+ANU)
      C2=C2*C3/(AN-ANU)
      FF=FF+(C2-C1)
      IF(ABS(C1)+ABS(C2).GE.0.5D-10*ABS(FF)) GO TO 100
      BESK=FF
      IF(LL.EQ.2) BESK=FF*EXP(X)
      GO TO 700
C------ ORDER CLOSE TO AN INTEGER ------
  200 LIBRTN=11
      IF(LIBMSGLVL.GE.1) WRITE(LIBMSG,220)
  220 FORMAT(' (BesK) NU is too close to an integer. ',
     %   'May not be accurate.')
      IF(NU.LE.1) THEN
        BESK=ABESK(0,X0,LL)
        IF(NU.EQ.1) BESK=BESK+ABESK(1,X0,LL)*(FRNU/X)
      ELSE
        F1=ABESK(0,X0,LL)
        F2=ABESK(1,X0,LL)
        XH=0.5D0*X
        XHI=1D0/XH
        AN=DFLOAT(NU)
        S=F1/AN
        C1=1D0
        AK=0D0
        DO 260 N=1,NU-1
          AK=AK+1D0
          C1=C1*XH/AK
          S=S+C1*F2/(AN-AK)
          F3=F2
          F2=F1+XHI*AK*F2
          F1=F3
 260    CONTINUE
        C1=AN/(C1*X)
        BESK=F2+C1*S*FRNU
      ENDIF
      GO TO 700
C------ DEBYE'S ASYMPTOTIC EXPANSION ------
  300 SQ=SQRT(ANUX)
C      F4=LOG((ANU+SQ)/X)*ANU-SQ
C      IF(F4.GT.+173.0) GO TO 920
C      IF(F4.LT.-173.0) GO TO 940
      F4=ANU*(ASINH(ANU/X)-ANU/(X+SQ))
      TT=1D0/SQ
      T=(ANU*TT)**2
      IF(P.LE.P2) GO TO 310
      F5=0.0
      IF(P.LE.P3) GO TO 320
      IF(P.LE.P4) GO TO 330
      IF(P.LE.P5) GO TO 340
      IF(P.LE.P6) GO TO 350
      GO TO 360
  310 C2=(((((UN( 1)*T+UN( 2))*T+UN( 3))*T+UN( 4))*T+UN( 5))*T+UN( 6))*T
     %        +UN( 7)
      F5=C2*TT
  320 C2= ((((UN( 8)*T+UN( 9))*T+UN(10))*T+UN(11))*T+UN(12))*T+UN(13)
      F5=(F5+C2)*TT
  330 C2=  (((UN(14)*T+UN(15))*T+UN(16))*T+UN(17))*T+UN(18)
      F5=(F5+C2)*TT
  340 C2=   ((UN(19)*T+UN(20))*T+UN(21))*T+UN(22)
      F5=(F5+C2)*TT
  350 C2=    (UN(23)*T+UN(24))*T+UN(25)
      F5=(F5+C2)*TT
  360 C2=     UN(26)*T+UN(27)
      F5=(F5+C2)*TT+1.0
      BESK=SQRT(HPAI*TT)*EXP(F4)*F5
      IF(LL.EQ.1) BESK=BESK*EXP(-X)
      GO TO 700
C------ INTEGER ORDER ------
  400 BESK=ABESK(NU,X0,LL)
      GO TO 700
C------ HALF INTEGER ORDER ------
  420 F7=SQRT(HPAI/X)
      IF(NU1.EQ.1) GO TO 460
      F6=F7
      DO 440 I=1,NU1-2,2
      F8=F7
      F7=F6+DFLOAT(I)*F7/X
  440 F6=F8
  460 BESK=F7
      IF(LL.EQ.1) BESK=BESK*EXP(-X)
      GO TO 700
C------ CONTINUED FRACTION ------
  500 XIH=0.5D0/X
      IF(X.LE.2.3D0*ANU+0.3D0) GO TO 520
      AN1=ANU
      KIN=1
      GO TO 600
  510 BESK=FF2
      GO TO 700
  520 IF(FRNU.GT.0.0) GO TO 530
      FRNU=FRNU+1D0
      NU=NU-1
  530 AN1=FRNU-1D0
      KIN=2
      GO TO 600
  540 FF1=FF2
      AN1=FRNU
      KIN=3
      GO TO 600
  550 XHI=2D0/X
      ANN=FRNU
      DO 560 I=1,NU
      FF3=FF2
      FF2=FF1+ANN*XHI*FF2
      FF1=FF3
  560 ANN=ANN+1D0
      BESK=FF2
      IF(LL.EQ.1) BESK=BESK*EXP(-X)
      GO TO 700
C          CONTINUED FRACTION (QUOTIENT-DIFFRENCE METHOD)
  600 ER=1.0D50
      AA=AN1*AN1-0.25D0
      A(1)=(0.5D0*AA-1D0)*XIH
      G0=1.0+AA*XIH 
      G1=G0-A(1)
      H0=1D0        
      H1=H0-A(1)
      FF2=G1/H1
      AN=1D0
      DO 630 N=2,20
      AN=AN+1D0
      A(N)=(AA/(AN+1.0)-AN)*XIH
      SAV2=0D0
      DO 620 I=1,N-1
      J=N-I
      SAV1=SAV2          
      SAV2=A(J)
      IF(MOD(I,2).NE.0) GO TO 610
      A(J)=SAV1/SAV2*A(J+1)
      GO TO 620
  610 A(J)=SAV1-SAV2+A(J+1)
  620 CONTINUE
      SAV=G1  
      G1=G1-A(1)*G0  
      G0=SAV
      SAV=H1  
      H1=H1-A(1)*H0  
      H0=SAV
      FSAV=FF2     
      FF2=G1/H1
      ESAV=ER      
      ER=ABS(FF2-FSAV)
      IF(ER+ESAV.LT.0.5D-7) GO TO 640
  630 CONTINUE
  640 FF2=SQRT(HPAI/X)*FF2
      GO TO (510,540,550), KIN
C------ NORMAL RETURN ------
  700 LIBRTN=0
      RETURN
C------ ERROR MESSAGES ------
  900 LIBRTN=1000
      IF(LIBMSGLVL.GE.0) WRITE(LIBMSG,905)
 905  FORMAT(' (BesK) 3rd argument must be 1 or 2.')
      RETURN
  910 LIBRTN=1000
      IF(LIBMSGLVL.GE.0) WRITE(LIBMSG,915)
 915  FORMAT(' (BesK) X must be >0.')
      RETURN
      END
C******************** ABESK *******************************************
      FUNCTION ABESK(N,X,LL)
C  Modified Bessel Function K of integer order
C  All REAL*8 but accuracy is REAL*4.
C       ABESK(N,X,1) = K(N,X)
C       ABESK(N,X,2) = K(N,X)*EXP(X)
      IMPLICIT NONE
      INTEGER N,LL
      REAL*8 ABESK,X
      INTEGER LIBRTN,LIBMSGLVL
      CHARACTER*80 LIBMSG
      COMMON/LIBCOM/LIBRTN,LIBMSGLVL
      COMMON/LIBCOM2/LIBMSG
      INTEGER NN,NM,I,N1,L,LX
      REAL*8 DX,RX,Z,HX,W,T1,T2,T3,S,FK,AIBES0,ABESK0,
     1       AIBES1,ABESK1,Y,EXPX
	INCLUDE 'include/ctrlcm.h'

      NN=IABS(N)
      IF( X .LE. 0D0 ) GO TO 900
      DX = X
      RX = 1.0D0 / DX
      Z=RX+RX
      IF(NN.GE.30000) GO TO 910
C      IF(X.GE.173.0D0) GO TO 103
      IF(X.GT.2D0) GO TO 150
      LX=1
      HX=0.5D0*DX
      W=DLOG(HX)
      IF(X.LT.2.0D-4) THEN
C-- Very small X
        ABESK0=-W-0.5772157D0
        IF(NN.NE.0) THEN
          ABESK1=RX-HX*ABESK0
          IF(NN.NE.1) THEN
            L=-69.0D0/W
            IF(NN.GT.L) GO TO 920
          ENDIF
        ENDIF
      ELSE
C-- Small X
        T3=0.0
        T2=1.0D-75
        S=0.0
        NM=12
        DO 100 I=1,NM
          FK=DFLOAT(NM-I+1)
          T1=FK*T2*Z+T3
          S=S+T1
          T3=T2
          T2=T1
 100    CONTINUE
        S=S+S-T2
        EXPX=DEXP(DX)
        AIBES0=T2/S*EXPX
        Y=HX*HX
        ABESK0=-W*AIBES0-0.5772157D0+(((((0.0000074D0*Y+0.0001075D0)*Y
     %    +0.0026270D0)*Y+0.0348859D0)*Y+0.2306976D0)*Y+0.4227842D0)*Y
        IF(NN.NE.0) THEN
          AIBES1=T3/S*EXPX
          ABESK1=(RX-AIBES1*ABESK0)/AIBES0
        ENDIF
      ENDIF
      GOTO 200
C-- Expansion for large X
 150  LX=2
      W=DSQRT(RX)
      IF(NN.NE.1) THEN
        ABESK0=((((((0.0005321D0*Z-0.0025154D0)*Z+0.0058787D0)*Z
     %  -0.0106245D0)*Z+0.0218957D0)*Z-0.0783236D0)*Z+1.2533141D0)*W
      ENDIF
      IF(NN.NE.0) THEN
        ABESK1=((((((-0.0006825D0*Z+0.0032561D0)*Z-0.0078035D0)*Z
     %  +0.0150427D0)*Z-0.0365562D0)*Z+0.2349862D0)*Z+1.2533141D0)*W
      ENDIF
C-- Reccurence relation
 200  IF(NN.EQ.0) THEN
        ABESK=ABESK0
        GOTO 600
      ELSEIF(NN.EQ.1) THEN
        ABESK=ABESK1
        GOTO 600
      ENDIF
      T1=ABESK0
      T2=ABESK1
      N1=NN-1
      DO 240 I=1,N1
        IF(T2.LT.1D65) THEN
          T3=DFLOAT(I)*T2*Z+T1
        ELSE
          W=T2*1.0D-10
          T3=DFLOAT(I)*W*Z+T1*1.0D-10
          IF(T3.GE.1.0D65) GO TO 920
          T3 = T3/1.0D-10
        ENDIF
        T1=T2
        T2=T3
 240  CONTINUE
      ABESK =T3
 600  IF(LX.EQ.1.AND.LL.EQ.2) THEN
        ABESK=ABESK*EXP(DX)
      ELSEIF(LX.EQ.2.AND.LL.EQ.1) THEN
        ABESK=ABESK*EXP(-DX)
      ENDIF
      RETURN
 900  ABESK=0
      LIBRTN=2000
      IF(LIBMSGLVL.GE.0) WRITE(LIBMSG,905)
 905  FORMAT(' (FUNC.ABESK) X must be >0.')
      RETURN
 910  ABESK=0.0
      LIBRTN=10
      IF(LIBMSGLVL.GE.1) WRITE(LIBMSG,915)
 915  FORMAT(' (FUNC.ABESK) Order N too large.')
      RETURN
 920  ABESK=0
      LIBRTN=1000
      IF(LIBMSGLVL.GE.0) WRITE(LIBMSG,925)
 925  FORMAT(' (FUNC.ABESK) Overflow')
      RETURN
      END

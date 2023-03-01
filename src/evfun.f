C------------------ EVFUN -------------------------
      SUBROUTINE EVFUN(IFUN,X,NV,Y,ERR)
	USE FLCHTYP
      IMPLICIT NONE
      INTEGER IFUN,NV
      TYPE(FLCHTYPE) X(NV),Y
      CHARACTER*(*) ERR
      INTEGER I
      REAL*8 DGAMMA
      REAL*8 ACOSH,ASINH,ATANH
C
	Y%L=1
	Y%X=0
      ERR=' '
      GOTO (10,20,30,40,  45, 50,  55,  60,  65,   70, 80, 90,
C            + - Int Nint Sgn Step Abs Frac Sqrt, Exp Log Log10 
     %  100,110,120, 130,   140,  150,
C       Cos Sin Tan ArcCos ArcSin ArcTan 
     %  160, 170, 180,  190,    200,    210,
C       Cosh Sinh Tanh ArcCosh ArcSinh ArcTanh 
     %  220,   230, 240,
C       Gamma  Mod  Atan2
     %  250, 260), IFUN
C       Min  Max
 10   Y%X=X(1)%X
      RETURN
 20   Y%X=-X(1)%X
      RETURN
 30   Y%X=INT(X(1)%X)
      RETURN
 40   Y%X=NINT(X(1)%X)
      RETURN
 45   IF(X(1)%X.GT.0) THEN
        Y%X=1
      ELSEIF(X(1)%X.EQ.0) THEN
        Y%X=0
      ELSE
        Y%X=-1
      ENDIF
      RETURN
 50   IF(X(1)%X.GE.0) THEN
        Y%X=1
      ELSE
        Y%X=0
      ENDIF
      RETURN
 55   Y%X=ABS(X(1)%X)
      RETURN
 60   IF(X(1)%X.GE.0) THEN
        Y%X=X(1)%X-INT(X(1)%X)
      ELSE
        Y%X=X(1)%X+INT(-X(1)%X)
      ENDIF
      RETURN
 65   IF(X(1)%X.GE.0) THEN
        Y%X=SQRT(X(1)%X)
      ELSE
        ERR='Negative number under square root.   (EVFUN)'
      ENDIF
      RETURN
 70   Y%X=EXP(X(1)%X)
      RETURN
 80   IF(X(1)%X.GT.0) THEN
        Y%X=LOG(X(1)%X)
      ELSE
        ERR='Non-positive number under log.   (EVFUN)'
      ENDIF
      RETURN
 90   IF(X(1)%X.GT.0) THEN
        Y%X=LOG10(X(1)%X)
      ELSE
        ERR='Non-positive number under log10.   (EVFUN)'
      ENDIF
      RETURN
 100  Y%X=COS(X(1)%X)
      RETURN
 110  Y%X=SIN(X(1)%X)
      RETURN
 120  Y%X=TAN(X(1)%X)
      RETURN
 130  IF(ABS(X(1)%X).LE.1) THEN
        Y%X=ACOS(X(1)%X)
      ELSE
        ERR='Abs[Argument of ArcCos] > 1.   (EVFUN)'
      ENDIF
      RETURN
 140  IF(ABS(X(1)%X).LE.1) THEN
        Y%X=ASIN(X(1)%X)
      ELSE
        ERR='Abs[Argument of ArcCos] > 1.   (EVFUN)'
      ENDIF
      RETURN
 150  Y%X=ATAN(X(1)%X)
      RETURN
 160  Y%X=COSH(X(1)%X)
      RETURN
 170  Y%X=SINH(X(1)%X)
      RETURN
 180  Y%X=TANH(X(1)%X)
      RETURN
 190  IF(X(1)%X.GE.1) THEN
        Y%X=ACOSH(X(1)%X)
      ELSE
        ERR='Argument of ArcCosh < 1.   (EVFUN)'
      ENDIF
      RETURN
 200  Y%X=ASINH(X(1)%X)
      RETURN
 210  IF(ABS(X(1)%X).LT.1) THEN
        Y%X=ATANH(X(1)%X)
      ELSE
        ERR='Abs[Argument of ArcTanh] > 1.   (EVFUN)'
      ENDIF
      RETURN
 220  IF(X(1)%X.LE.0.AND.ABS(X(1)%X-NINT(X(1)%X)).LE.1D-14) THEN
        ERR='Gamma function for non-positive integer.   (EVFUN)'
      ELSE
        Y%X=DGAMMA(X(1)%X)
      ENDIF
      RETURN
 230  IF(ABS(X(2)%X).EQ.0) THEN
        ERR='2nd arg.of Mod=0.   (EVFUN)'
      ELSE
        Y%X=MOD(X(1)%X,X(2)%X)
      ENDIF
      RETURN
 240  IF(X(1)%X.EQ.0.AND.X(2)%X.EQ.0) THEN
        ERR='Argument of Atan2 = (0,0).   (EVFUN)'
      ELSE
        Y%X=ATAN2(X(1)%X,X(2)%X)
      ENDIF
      RETURN
 250  IF(NV.LE.0) THEN
        ERR='No argument for Min.         (EVFUN)'
      ELSE
        Y%X=X(1)%X
        IF(NV.GE.2) THEN
          DO 252 I=2,NV
            Y%X=MIN(Y%X,X(I)%X)
 252      CONTINUE
        ENDIF
      ENDIF
      RETURN
 260  IF(NV.LE.0) THEN
        ERR='No argument for Max.         (EVFUN)'
      ELSE
        Y%X=X(1)%X
        IF(NV.GE.2) THEN
          DO 262 I=2,NV
            Y%X=MAX(Y%X,X(I)%X)
 262      CONTINUE
        ENDIF
      ENDIF
      RETURN
      END

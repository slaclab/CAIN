C************************ BKI13 **************************************
      FUNCTION BKI13(X,L)
C  Integral of modified Bessel function K(1/3).  REAL*8, fast.
C  F(x)= Integral K(1/3,y) dy from y=x to infty.
C   BKI13(X,1)=F(X)
C   BKI13(X,2)=exp(X)*F(X)
C
C   Relative error < 1.02E-4  for all x.
C
      IMPLICIT NONE
      INTEGER L
      REAL*8 BKI13,X
      REAL*8 A1/1.8136145D0/,A2/-2.5278090D0/,A3/1.1979670D0/,
     %       A4/-0.20490413D0/,A5/0.058671692D0/
      REAL*8 B1/1.2531864D0/,B2/1.6215470D0/,
     %       B3/1.8548547D0/,B4/0.28146211D0/
      REAL*8 X0/1.2777D0/,X23,X2,Y
	INCLUDE 'include/ctrlcm.h'
C
      BKI13=0
      IF(X.LT.0.OR.L.LE.0.OR.L.GE.3) GOTO 900
      IF(X.LT.X0) THEN
        IF(X.EQ.0) THEN
          BKI13=A1
        ELSE
          X23=X**(0.666666667D0)
          X2=X**2
          BKI13=A1+X23*(A2+A4*X2+X23*(A3+A5*X2))
          IF(L.EQ.2) BKI13=BKI13*EXP(X)
        ENDIF
      ELSE
        Y=1/X
        BKI13=SQRT(Y)*(B1+B2*Y)/(1D0+Y*(B3+Y*B4))
        IF(L.EQ.1) THEN
          IF(X.GE.130) THEN
            BKI13=0
          ELSE
            BKI13=BKI13*EXP(-X)
          ENDIF
        ENDIF
      ENDIF
      RETURN
  900 WRITE(MSGFL,910)
  910 FORMAT(' (FUNC.BKI13) INVALID ARGUMENT.')
      RETURN
      END

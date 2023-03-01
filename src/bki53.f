C************************ BKI53 **************************************
      FUNCTION BKI53(X,L)
C  Integral of modified Bessel function K(5/3).  REAL*8, fast.
C  F(x)= Integral K(5/3,y) dy from y=x to infty.
C   BKI53(X,1)=F(X)
C   BKI53(X,2)=exp(X)*F(X)
C
C   Relative error < 1.08E-4  for all x.
C
      IMPLICIT NONE
      INTEGER L
      REAL*8 BKI53,X
      REAL*8 A1/2.14929687D0/,A2/-1.81166672D0/,A3/0.392081416D0/,
     %       A4/-0.110294346D0/,A5/0.03204274625D0/
      REAL*8 B1/1.25344888D0/,B2/2.52417499D0/,B3/1.26015974D0/,
     %       B4/-0.135048010D0/
      REAL*8 X0/1.605D0/,X23,X43,Y
	INCLUDE 'include/ctrlcm.h'
C
      BKI53=0
      IF(X.LE.0.OR.L.LE.0.OR.L.GE.3) GOTO 900
      IF(X.LT.X0) THEN
        X23=X**(0.666666667D0)
        X43=X23**2
        BKI53=A1/X23+A2+X43*(A3+X43*(A4+X23*A5))
        IF(L.EQ.2) BKI53=BKI53*EXP(X)
      ELSE
        Y=1/X
        BKI53=SQRT(Y)*(B1+B2*Y)/(1D0+Y*(B3+Y*B4))
        IF(L.EQ.1) THEN
          IF(X.GE.130) THEN
            BKI53=0
          ELSE
            BKI53=BKI53*EXP(-X)
          ENDIF
        ENDIF
      ENDIF
      RETURN
  900 WRITE(MSGFL,910)
  910 FORMAT(' (FUNC.BKI53) INVALID ARGUMENT.')
      RETURN
      END

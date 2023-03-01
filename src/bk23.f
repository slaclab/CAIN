C************************ BK23 **************************************
      FUNCTION BK23(X,L)
C  Modified Bessel function K(2/3,X).  REAL*8, fast.
C   BK23(X,1)=K(2/3,X)
C   BK23(X,2)=exp(X)*K(2/3,X)
C
C   Relative error < 0.387E-4  for all x.
C
      IMPLICIT NONE
      INTEGER L
      REAL*8 BK23,X
      REAL*8 A1/1.07472258D0/,A2/-1.25983918D0/,
     %       A3/0.78389624D0/,A4/-0.1110287536D0/,
     %       B1/1.25336258D0/,B2/0.726128384D0/,
     %       B3/0.4834810215D0/,B4/-0.0107682563D0/
      REAL*8 X0/0.60406D0/,X23,X2,Y
	INCLUDE 'include/ctrlcm.h'
C
      BK23=0
      IF(X.LE.0.OR.L.LE.0.OR.L.GE.3) GOTO 900
      IF(X.LT.X0) THEN
        X23=X**(0.666666667D0)
        X2=X**2
        BK23=(A1+A3*X2)/X23+(A2+A4*X2)*X23
        IF(L.EQ.2) BK23=BK23*EXP(X)
      ELSE
        Y=1/X
        BK23=SQRT(Y)*(B1+B2*Y)/(1D0+Y*(B3+Y*B4))
        IF(L.EQ.1) THEN
          IF(X.GE.130) THEN
            BK23=0
          ELSE
            BK23=BK23*EXP(-X)
          ENDIF
        ENDIF
      ENDIF
      RETURN
  900 WRITE(MSGFL,910)
  910 FORMAT(' (FUNC.BK23) INVALID ARGUMENT.')
      RETURN
      END

C************************ BK13 **************************************
      FUNCTION BK13(X,L)
C  MODIFIED BESSEL FUNCTION K(1/3,X).  REAL*8, FAST.
C   BK13(X,1)=K(1/3,X)
C   BK13(X,2)=EXP(X)*K(1/3,X)
C
C   RELATIVE ERROR < 0.325E-4  FOR ALL X.
C
      IMPLICIT NONE
      INTEGER L
      REAL*8 BK13,X
      REAL*8 A1/1.687570834D0/,A2/-1.611154585D0/,
     %       A3/0.611515182D0/,A4/-0.256982212D0/,
     %       B1/1.253273433D0/,B2/ 0.671444784D0/,
     %       B3/0.604094190D0/,B4/ 0.010909437D0/
      REAL*8 X0/0.546D0/,X13,X2,Y
	INCLUDE 'include/ctrlcm.h'
C
      BK13=0
      IF(X.LE.0.OR.L.LE.0.OR.L.GE.3) GOTO 900
      IF(X.LT.X0) THEN
        X13=X**(0.3333333333D0)
        X2=X**2
        BK13=(A1+A3*X2)/X13+(A2+A4*X2)*X13
        IF(L.EQ.2) BK13=BK13*EXP(X)
      ELSE
        Y=1/X
        BK13=SQRT(Y)*(B1+B2*Y)/(1D0+Y*(B3+Y*B4))
        IF(L.EQ.1) THEN
          IF(X.GE.130) THEN
            BK13=0
          ELSE
            BK13=BK13*EXP(-X)
          ENDIF
        ENDIF
      ENDIF
      RETURN
  900 WRITE(MSGFL,910)
  910 FORMAT(' (FUNC.BK13) INVALID ARGUMENT.')
      RETURN
      END

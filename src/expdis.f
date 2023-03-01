      SUBROUTINE EXPDIS(X,N,GCUT)
C  Random number generator of exponential distribution  
C  f(x) proportional to exp(-x) cut at x=GCUT**2/2
      IMPLICIT NONE
      INTEGER N
      REAL*8 X(N),GCUT
      INTEGER I
      REAL*8 C
      IF(ABS(GCUT).LE.10D0) THEN
        C=1-EXP(-GCUT**2/2)
      ELSE
        C=1
      ENDIF
      CALL RANDN(X,N)
      DO 200 I=1,N
        X(I)=1-C*X(I)
        IF(X(I).GT.0) THEN
          X(I)=-LOG(X(I))
        ELSE
          X(I)=0
C           (This is possible only due to round off error)
        ENDIF
 200  CONTINUE
      RETURN
      END

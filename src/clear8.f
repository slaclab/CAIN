      SUBROUTINE CLEAR8(X,N)
      IMPLICIT NONE
      INTEGER N,I
      REAL*8 X(N)
      DO 100 I=1,N
        X(I)=0
 100  CONTINUE
      RETURN
      END

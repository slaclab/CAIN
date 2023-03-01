      SUBROUTINE MATVEC(A,M,N,X)
      IMPLICIT NONE
      INTEGER M,N,I,J
      REAL*8 A(M,N),X(N),W(10)
	INCLUDE 'include/ctrlcm.h'
      IF(N.GT.10) THEN
        WRITE(MSGFL,100)
 100    FORMAT(' PROGRAM ERROR IN LORNTZ/MATVEC')
        CALL STOPCAIN(100)
      ENDIF
      DO 200 I=1,N
        W(I)=0
        DO 180 J=1,N
          W(I)=W(I)+A(I,J)*X(J)
 180    CONTINUE
 200  CONTINUE
      DO 220 I=1,N
        X(I)=W(I)
 220  CONTINUE
      RETURN
      END

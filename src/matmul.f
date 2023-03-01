      SUBROUTINE MATMUL(N,A,MA,B,MB,C,MC)
C  Matrix multiplication.  Large matrix nont intended. upto 100*100.
C  Memory area for A,B,C may overlap. 
      IMPLICIT NONE
      INTEGER N,MA,MB,MC
      REAL*8 A(MA,N),B(MB,N),C(MC,N)
      INTEGER M,I,J,K
      PARAMETER (M=100)
      REAL*8 W(M)
	INCLUDE 'include/ctrlcm.h'
C
      IF(N.GT.M) THEN
        WRITE(MSGFL,100)
 100    FORMAT(' **** PROGRAM ERROR IN SUBR MATMUL *****')
        CALL STOPCAIN(100)
      ENDIF
      DO 200 J=1,N
        DO 160 I=1,N
          W(I)=0
          DO 140 K=1,N
            W(I)=W(I)+A(I,K)*B(K,J)
 140      CONTINUE
 160    CONTINUE
        DO 180 I=1,N
          C(I,J)=W(I)
 180    CONTINUE
 200  CONTINUE
      RETURN
      END

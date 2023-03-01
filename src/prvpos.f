      SUBROUTINE PRVPOS(L,N,L1,N1,NCHAR,IRTN)
      IMPLICIT NONE
      INTEGER L,N,L1,N1,NCHAR(L1),IRTN
      INTEGER L0,N0,NN
C
      L0=L
      N0=N
      IF(L0.EQ.L1) THEN
        NN=N1
      ELSE
        NN=1
      ENDIF
      IF(N0.GT.NN) THEN
        N0=N0-1
      ELSE
 200    L0=L0-1
        IF(L0.LE.0) GOTO 300
        IF(NCHAR(L0).EQ.0) GOTO 200
        N0=NCHAR(L0)
      ENDIF
      IRTN=0
      L=L0
      N=N0
      RETURN
 300  IRTN=1
      RETURN
      END


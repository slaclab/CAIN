      SUBROUTINE NXTPOS(L,N,L2,N2,NCHAR,IRTN)
      IMPLICIT NONE
      INTEGER L,N,L2,N2,NCHAR(L2),IRTN
      INTEGER L0,N0,NN
C
      L0=L
      N0=N
      IF(L0.EQ.L2) THEN
        NN=N2
      ELSE
        NN=NCHAR(L0)
      ENDIF
      IF(N0.LT.NN) THEN
        N0=N0+1
      ELSE
 200    L0=L0+1
        IF(L0.GT.L2) GOTO 300
        IF(NCHAR(L0).EQ.0) GOTO 200
        N0=1
      ENDIF
      IRTN=0
      L=L0
      N=N0
      RETURN
 300  IRTN=1
      RETURN
      END

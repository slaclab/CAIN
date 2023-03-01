      SUBROUTINE PRECHO(L1,N1,L2,N2,LINE,NCHAR)
C  Print input data from (L1,N1) to (L2,N2)
C  without comment part
      IMPLICIT NONE
      INTEGER L1,N1,L2,N2,NCHAR(*)
      CHARACTER*(*) LINE(*)
      INTEGER L,NN1,NN2
      INCLUDE 'include/ctrlcm.h'
C
      DO 300 L=L1,L2
        IF(L.EQ.L1) THEN
          NN1=N1
          IF(L.EQ.L2) THEN
            NN2=N2
          ELSE
            NN2=NCHAR(L)
          ENDIF
        ELSE
          NN1=1
          IF(L.EQ.L2) THEN
            NN2=N2
          ELSE
            NN2=NCHAR(L)
          ENDIF
        ENDIF
        WRITE(MSGFL,'(A)') LINE(L)(NN1:NN2)
 300  CONTINUE
      RETURN
      END
